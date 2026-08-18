import './foliate-js/view.js'
import { EPUB } from './foliate-js/epub.js'
import {
    collapse as collapseCFI,
    fromRangeEndpoints,
} from './foliate-js/epubcfi.js'
import { searchMatcher } from './foliate-js/search.js'
import { textWalker } from './foliate-js/text-walker.js'

const BOOK_ROOT = 'https://yunge-reader-book.localhost/'
const CATALOG_PATH = '.yunge/resources.json'
const CONTENT_CSP = [
    "default-src 'none'",
    "img-src blob: data:",
    "style-src blob: 'unsafe-inline'",
    "font-src blob: data:",
    "media-src blob: data:",
    "script-src 'none'",
    "object-src 'none'",
    "frame-src 'none'",
    "connect-src 'none'",
    "base-uri 'none'",
    "form-action 'none'",
].join('; ')

const reader = document.querySelector('#reader')
const status = document.querySelector('#status')
const decoder = new TextDecoder()
const encoder = new TextEncoder()
const MAX_INITIAL_TARGETS = 8
const MAX_LOCATOR_TEXT_BYTES = 3072
const MAX_SELECTION_CHARACTERS = 1024 * 1024
const MAX_SELECTION_CHARACTER_LIMIT = 65536
const MAX_SEARCH_QUERY_CHARACTERS = 256
const MAX_SEARCH_MATCH_LIMIT = 200
const MAX_SEARCH_SECTION_LIMIT = 64
const MAX_SEARCH_CURSOR_OFFSET = 1024 * 1024
const MAX_SEARCH_RESULT_BYTES = 384 * 1024
const SEARCH_HIGHLIGHT_COLOR = '#ff7800'
const MAX_MEDIA_CANDIDATES = 256
const MAX_TEXT_NODES = 4096
const MAX_TEXT_SAMPLE = 4096
const MAX_TOC_ITEMS = 4096
const MAX_TOC_DEPTH = 256
const MAX_TOC_HREF_BYTES = 3072
const MAX_TOC_TITLE_BYTES = 1024
const MAX_TOC_TOTAL_TEXT_BYTES = 384 * 1024
const MAX_EXTERNAL_URI_BYTES = 4096
const MIN_FIXED_SCALE = 0.25
const MAX_FIXED_SCALE = 8.0
const MAX_VIEWPORT_COORDINATE = 1000000
const LOCATION_DELAY_MS = 75
const USER_MOVEMENT_WINDOW_MS = 1000
const READER_CHARACTER_KEYS = Object.freeze([
    '+', '-', '=', 'G', 'J', 'K', 'SPC', 'g', 'j', 'k', 'y',
])
const SELECTION_TEXT_ERROR_MESSAGES = Object.freeze({
    'invalid-selection-offset':
        'EPUB selection offset lies outside the selection',
    'selection-no-longer-current':
        'EPUB selection is no longer current',
    'selection-too-large':
        'EPUB selection exceeds its character limit',
    'selection-unavailable':
        'EPUB selection range is unavailable',
})
const SEARCH_ERROR_MESSAGES = Object.freeze({
    'invalid-search-cursor': 'EPUB search cursor is invalid',
    'search-result-too-large': 'EPUB search result exceeds its byte limit',
    'search-unavailable': 'EPUB search is unavailable',
})
const DEFAULT_STYLE = Object.freeze({
    'font-scale': 1.0,
    'line-height': 1.6,
    'content-width': 720,
    'side-padding': 7.0,
})
let generation = 0
let current = null

const post = (event, {
    message, location, outline, selection, key, uri, user, scale,
} = {}) => {
    const payload = { protocol: 1, event }
    if (message) payload.message = String(message).slice(0, 4096)
    if (location) payload.location = location
    if (outline) payload.outline = outline
    if (selection !== undefined) payload.selection = selection
    if (key) payload.key = key
    if (uri) payload.uri = uri
    if (typeof user === 'boolean') payload.user = user
    if (typeof scale === 'number') payload.scale = scale
    window.ipc.postMessage(JSON.stringify(payload))
}

const checkedExternalURI = value => {
    if (typeof value !== 'string' || !value
        || encoder.encode(value).length > MAX_EXTERNAL_URI_BYTES
        || /[\s\p{Cc}]/u.test(value)
        || !/^[A-Za-z][A-Za-z0-9+.-]*:/u.test(value)) {
        throw new Error('Invalid EPUB external URI')
    }
    return value
}

const checkedRendererAccelerators = value => {
    if (!Array.isArray(value)
        || value.length !== READER_CHARACTER_KEYS.length
        || value.some((key, index) =>
            key !== READER_CHARACTER_KEYS[index])) {
        throw new Error('Incompatible EPUB renderer accelerator contract')
    }
    return value
}

const postSearchResult = (request, response) => {
    window.ipc.postMessage(JSON.stringify({
        protocol: 1,
        event: 'search-result',
        request,
        response,
    }))
}

const readerCharacterKey = event => {
    if (event.defaultPrevented || event.isComposing
        || event.ctrlKey || event.altKey || event.metaKey
        || event.target?.closest?.(
            'input, textarea, select, '
            + '[contenteditable]:not([contenteditable="false"])')) {
        return null
    }
    if (event.code === 'Space') return 'SPC'
    return READER_CHARACTER_KEYS.includes(event.key)
        ? event.key : null
}

const relayReaderCharacterKey = event => {
    const key = readerCharacterKey(event)
    if (!key) return
    event.preventDefault()
    event.stopImmediatePropagation()
    if ((key === 'SPC' || key === 'g') && event.repeat) return
    post('accelerator', { key })
}

const installReaderCharacterKeys = target => {
    target.addEventListener('keydown', relayReaderCharacterKey, true)
}

const installLocationActivity = (target, session) => {
    const note = event => {
        if (event.type !== 'pointermove' || event.buttons) {
            session.userMovementDeadline = performance.now()
                + USER_MOVEMENT_WINDOW_MS
        }
    }
    for (const type of ['wheel', 'pointerdown', 'pointermove', 'touchstart']) {
        target.addEventListener(type, note, { capture: true, passive: true })
    }
}

installReaderCharacterKeys(document)

const encodePath = path => path.split('/').map(encodeURIComponent).join('/')

const checkedRoot = value => {
    const url = new URL(value)
    if (url.origin !== BOOK_ROOT.slice(0, -1)
        || !/^\/[0-9a-f]{32}\/$/.test(url.pathname)) {
        throw new Error('Invalid Yunge Reader publication resource root')
    }
    return url.href
}

const checkedView = value => {
    if (!Number.isSafeInteger(value) || value < 1) {
        throw new Error('Invalid Yunge Reader view identifier')
    }
    return value
}

const checkedStyle = value => {
    value ??= DEFAULT_STYLE
    const keys = value && typeof value === 'object' && !Array.isArray(value)
        ? Object.keys(value) : []
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || keys.length !== 4
        || keys.some(key => ![
            'font-scale', 'line-height', 'content-width', 'side-padding',
        ].includes(key))) {
        throw new Error('Invalid EPUB reading style')
    }
    const fontScale = value['font-scale']
    const lineHeight = value['line-height']
    const contentWidth = value['content-width']
    const sidePadding = value['side-padding']
    if (!Number.isFinite(fontScale) || fontScale < 0.5 || fontScale > 3
        || !Number.isFinite(lineHeight)
        || lineHeight < 1 || lineHeight > 3
        || !Number.isSafeInteger(contentWidth)
        || contentWidth < 320 || contentWidth > 1600
        || !Number.isFinite(sidePadding)
        || sidePadding < 0 || sidePadding > 20) {
        throw new Error('Invalid EPUB reading style')
    }
    return Object.freeze({
        fontScale,
        lineHeight,
        contentWidth,
        sidePadding,
    })
}

const applyReadingStyle = (view, style) => {
    if (view.isFixedLayout) return
    view.renderer.setAttribute('max-inline-size',
        `${style.contentWidth}px`)
    view.renderer.setAttribute('gap', `${style.sidePadding}%`)
    view.renderer.setStyles(`
        body {
            font-size: ${style.fontScale}em !important;
            line-height: ${style.lineHeight} !important;
        }
        p, li, blockquote, dd {
            line-height: ${style.lineHeight} !important;
        }
    `)
}

const checkedZoom = value => {
    if (value === 'fit-width' || value === 'fit-page') return value
    if (!Number.isFinite(value)
        || value < MIN_FIXED_SCALE || value > MAX_FIXED_SCALE) {
        throw new Error('Invalid EPUB fixed-layout zoom')
    }
    return value
}

const applyFixedZoom = (view, zoom) => {
    if (!view.isFixedLayout) {
        throw new Error('Cannot zoom a reflowable EPUB as fixed layout')
    }
    view.renderer.setAttribute('zoom', String(zoom))
}

const checkedScrollBars = value => {
    if (typeof value !== 'boolean') {
        throw new Error('Invalid EPUB scroll bar visibility')
    }
    return value
}

const applyScrollBars = (view, visible) => {
    view.renderer.setAttribute(
        'scroll-bars', visible ? 'visible' : 'hidden')
}

const sameReadingStyle = (left, right) => left && right
    && left.fontScale === right.fontScale
    && left.lineHeight === right.lineHeight
    && left.contentWidth === right.contentWidth
    && left.sidePadding === right.sidePadding

const applyPendingStyle = session => {
    session.styleFrame = null
    const style = session.pendingStyle
    session.pendingStyle = null
    if (current !== session || !style
        || sameReadingStyle(session.style, style)) return
    try {
        applyReadingStyle(session.view, style)
        session.style = style
    } catch (error) {
        post('style-error', { message: error?.message ?? error })
    }
}

const setStyle = ({ view: viewID, style }) => {
    try {
        viewID = checkedView(viewID)
        style = checkedStyle(style)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        if (current.view.isFixedLayout) {
            throw new Error('Cannot style a fixed-layout EPUB as reflowable')
        }
        const session = current
        session.pendingStyle = style
        session.styleFrame ??= requestAnimationFrame(
            () => applyPendingStyle(session))
    } catch (error) {
        post('style-error', { message: error?.message ?? error })
    }
}

const setZoom = ({ view: viewID, zoom }) => {
    try {
        viewID = checkedView(viewID)
        zoom = checkedZoom(zoom)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        applyFixedZoom(current.view, zoom)
        current.zoom = zoom
    } catch (error) {
        post('zoom-error', { message: error?.message ?? error })
    }
}

const setScrollBars = ({ view: viewID, visible }) => {
    try {
        viewID = checkedView(viewID)
        visible = checkedScrollBars(visible)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        applyScrollBars(current.view, visible)
    } catch (error) {
        post('scroll-bars-error', { message: error?.message ?? error })
    }
}

const checkedLocatorText = (value, name) => {
    if (typeof value !== 'string' || !value
        || encoder.encode(value).length > MAX_LOCATOR_TEXT_BYTES
        || /[\u0000-\u001f\u007f]/u.test(value)) {
        throw new Error(`Invalid EPUB locator ${name}`)
    }
    return value
}

const checkedLocator = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        throw new Error('Invalid EPUB locator')
    }
    const keys = Object.keys(value)
    if (keys.some(key => ![
        'cfi', 'href', 'fraction', 'x', 'y',
    ].includes(key))) {
        throw new Error('Invalid EPUB locator field')
    }
    const cfi = checkedLocatorText(value.cfi, 'CFI')
    const href = checkedLocatorText(value.href, 'href')
    if (!/^epubcfi\(.+\)$/u.test(cfi)) {
        throw new Error('Invalid EPUB locator CFI')
    }
    if (href.startsWith('/') || /[\\:?#]/u.test(href)
        || href.split('/').some(part => !part || ['.', '..'].includes(part))) {
        throw new Error('Invalid EPUB locator href')
    }
    const result = { cfi, href }
    if (value.fraction !== undefined && value.fraction !== null) {
        if (!Number.isFinite(value.fraction)
            || value.fraction < 0 || value.fraction > 1) {
            throw new Error('Invalid EPUB locator fraction')
        }
        result.fraction = value.fraction
    }
    const hasX = value.x !== undefined && value.x !== null
    const hasY = value.y !== undefined && value.y !== null
    if (hasX !== hasY) {
        throw new Error('Incomplete EPUB viewport coordinate')
    }
    if (hasX) {
        for (const coordinate of [value.x, value.y]) {
            if (!Number.isFinite(coordinate) || coordinate < 0
                || coordinate > MAX_VIEWPORT_COORDINATE) {
                throw new Error('Invalid EPUB viewport coordinate')
            }
        }
        result.x = value.x
        result.y = value.y
    }
    return Object.freeze(result)
}

const checkedSelection = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        throw new Error('Invalid EPUB selection')
    }
    const keys = Object.keys(value).sort()
    if (keys.join() !== 'end,href,start') {
        throw new Error('Invalid EPUB selection field')
    }
    const href = checkedLocatorText(value.href, 'href')
    const start = checkedLocatorText(value.start, 'selection start')
    const end = checkedLocatorText(value.end, 'selection end')
    for (const cfi of [start, end]) {
        if (!/^epubcfi\(.+\)$/u.test(cfi)) {
            throw new Error('Invalid EPUB selection CFI')
        }
    }
    if (href.startsWith('/') || /[\\:?#]/u.test(href)
        || href.split('/').some(part => !part
            || ['.', '..'].includes(part))
        || start === end) {
        throw new Error('Invalid EPUB selection range')
    }
    return Object.freeze({ href, start, end })
}

const selectionTextError = (code, message) => {
    const error = new Error(message)
    error.code = code
    throw error
}

const searchError = (code, message) => {
    const error = new Error(message)
    error.code = code
    throw error
}

const checkedSelectionTextRequest = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).sort().join()
            !== 'character-limit,offset,selection,view') {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection text request is invalid')
    }
    const viewID = checkedView(value.view)
    const selection = checkedSelection(value.selection)
    const offset = value.offset
    const characterLimit = value['character-limit']
    if (!Number.isSafeInteger(offset) || offset < 0
        || offset > MAX_SELECTION_CHARACTERS
        || !Number.isSafeInteger(characterLimit)
        || characterLimit < 1
        || characterLimit > MAX_SELECTION_CHARACTER_LIMIT) {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection text request is invalid')
    }
    return { viewID, selection, offset, characterLimit }
}

const checkedSearchCursor = value => {
    if (value === null) return null
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).sort().join() !== 'href,offset') {
        searchError(
            'invalid-search-cursor',
            'EPUB search cursor is invalid')
    }
    const href = checkedLocatorText(value.href, 'search href')
    const offset = value.offset
    if (!checkedOutlineHref(href) || href.includes('#')
        || (offset !== null
            && (!Number.isSafeInteger(offset) || offset < 0
                || offset > MAX_SEARCH_CURSOR_OFFSET))) {
        searchError(
            'invalid-search-cursor',
            'EPUB search cursor is invalid')
    }
    return Object.freeze({ href, offset })
}

const checkedSearchRequest = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).sort().join()
            !== 'case-sensitive,cursor,direction,match-limit,origin,'
                + 'query,request,section-limit,view') {
        searchError('search-unavailable', 'EPUB search request is invalid')
    }
    const viewID = checkedView(value.view)
    const request = value.request
    const query = value.query
    const caseSensitive = value['case-sensitive']
    const direction = value.direction
    let origin
    try {
        origin = value.origin === null ? null : checkedLocator(value.origin)
    } catch (_) {
        searchError(
            'invalid-search-cursor',
            'EPUB search origin is invalid')
    }
    const cursor = checkedSearchCursor(value.cursor)
    const matchLimit = value['match-limit']
    const sectionLimit = value['section-limit']
    if (!Number.isSafeInteger(request) || request < 1
        || typeof query !== 'string' || !Array.from(query).length
        || Array.from(query).length > MAX_SEARCH_QUERY_CHARACTERS
        || /[\p{Control}]/u.test(query)
        || typeof caseSensitive !== 'boolean'
        || !['forward', 'backward'].includes(direction)
        || (origin !== null && cursor !== null)
        || !Number.isSafeInteger(matchLimit) || matchLimit < 1
        || matchLimit > MAX_SEARCH_MATCH_LIMIT
        || !Number.isSafeInteger(sectionLimit) || sectionLimit < 1
        || sectionLimit > MAX_SEARCH_SECTION_LIMIT) {
        searchError('search-unavailable', 'EPUB search request is invalid')
    }
    return {
        request, viewID, query, caseSensitive, direction, origin,
        cursor, matchLimit, sectionLimit,
    }
}

const checkedSearchResultRequest = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).sort().join() !== 'reveal,selection,view') {
        throw new Error('Invalid EPUB search result request')
    }
    const viewID = checkedView(value.view)
    const selection = value.selection === null
        ? null : checkedSelection(value.selection)
    if (typeof value.reveal !== 'boolean') {
        throw new Error('Invalid EPUB search result reveal flag')
    }
    return { viewID, selection, reveal: value.reveal }
}

const checkedSetSelectionRequest = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || Object.keys(value).sort().join() !== 'selection,view') {
        throw new Error('Invalid EPUB set selection request')
    }
    return {
        viewID: checkedView(value.view),
        selection: checkedSelection(value.selection),
    }
}

const checkedNavigationTarget = value => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        throw new Error('Invalid EPUB navigation target')
    }
    if (value.cfi !== undefined) return checkedLocator(value)
    const keys = Object.keys(value)
    const href = checkedOutlineHref(value.href)
    if (keys.length !== 1 || keys[0] !== 'href' || !href) {
        throw new Error('Invalid EPUB navigation target')
    }
    return Object.freeze({ href })
}

const fetchResource = async (root, path) => {
    const response = await fetch(root + encodePath(path), {
        cache: 'no-store',
        credentials: 'omit',
        redirect: 'error',
        referrerPolicy: 'no-referrer',
    })
    if (response.status === 404) return null
    if (!response.ok) {
        throw new Error(
            `Could not load EPUB resource ${path}: ${response.status}`)
    }
    return response
}

const makeLoader = async root => {
    const response = await fetchResource(root, CATALOG_PATH)
    if (!response) throw new Error('EPUB resource catalog is unavailable')
    const catalog = await response.json()
    if (!Array.isArray(catalog.resources)) {
        throw new Error('EPUB resource catalog is invalid')
    }
    const sizes = new Map(catalog.resources.map(resource => [
        resource.path,
        Number.isSafeInteger(resource.size) ? resource.size : 0,
    ]))
    const loadBlob = async path => {
        const resource = await fetchResource(root, path)
        return resource?.blob() ?? null
    }
    const loadText = async path => {
        const resource = await fetchResource(root, path)
        return resource ? decoder.decode(await resource.arrayBuffer()) : null
    }
    return {
        getSize: path => sizes.get(path) ?? 0,
        loadBlob,
        loadText,
    }
}

const addContentPolicy = (data, type) => {
    if (typeof data !== 'string'
        || !['application/xhtml+xml', 'text/html'].includes(type)) {
        return data
    }
    const doc = new DOMParser().parseFromString(data, type)
    const namespace = doc.documentElement?.namespaceURI
    let head = doc.querySelector('head')
    if (!head) {
        head = namespace
            ? doc.createElementNS(namespace, 'head')
            : doc.createElement('head')
        doc.documentElement?.prepend(head)
    }
    const meta = namespace
        ? doc.createElementNS(namespace, 'meta')
        : doc.createElement('meta')
    meta.setAttribute('http-equiv', 'Content-Security-Policy')
    meta.setAttribute('content', CONTENT_CSP)
    head.prepend(meta)
    return new XMLSerializer().serializeToString(doc)
}

const protectBook = book => {
    book.transformTarget?.addEventListener('load', event => {
        if (event.detail.isScript) event.detail.allow = false
    })
    book.transformTarget?.addEventListener('data', event => {
        event.detail.data = Promise.resolve(event.detail.data).then(data =>
            addContentPolicy(data, event.detail.type))
    })
}

const closeCurrent = () => {
    if (!current) return
    if (current.locationTimer) clearTimeout(current.locationTimer)
    current.pendingNavigation = null
    if (current.styleFrame !== null) {
        cancelAnimationFrame(current.styleFrame)
    }
    if (current.selectionFrame !== null) {
        cancelAnimationFrame(current.selectionFrame)
    }
    current.view.close()
    current.book.destroy()
    current.view.remove()
    current = null
}

const boundedOutlineTitle = value => {
    if (typeof value !== 'string') return { value: null, truncated: false }
    value = value.replace(/[\u0000-\u001f\u007f]/gu, ' ')
        .replace(/\s+/gu, ' ').trim()
    if (!value) return { value: null, truncated: false }
    if (encoder.encode(value).length <= MAX_TOC_TITLE_BYTES) {
        return { value, truncated: false }
    }
    let result = ''
    let bytes = 0
    for (const character of value) {
        const size = encoder.encode(character).length
        if (bytes + size > MAX_TOC_TITLE_BYTES) break
        result += character
        bytes += size
    }
    return { value: result.trim(), truncated: true }
}

const checkedOutlineHref = value => {
    if (typeof value !== 'string' || !value
        || encoder.encode(value).length > MAX_TOC_HREF_BYTES
        || /[\u0000-\u001f\u007f\\?]/u.test(value)) return null
    const hash = value.indexOf('#')
    const path = hash < 0 ? value : value.slice(0, hash)
    if (!path || path.startsWith('/') || path.includes(':')
        || (hash >= 0 && value.indexOf('#', hash + 1) >= 0)
        || path.split('/').some(part =>
            !part || ['.', '..'].includes(part))) return null
    return value
}

const outlineFromBook = toc => {
    const items = []
    const roots = Array.isArray(toc) ? toc : []
    const stack = []
    let truncated = roots.length > MAX_TOC_ITEMS
    let textBytes = 0
    let seen = 0
    const pushChildren = (children, depth) => {
        if (!Array.isArray(children) || !children.length) return
        const available = Math.max(0, MAX_TOC_ITEMS - stack.length)
        const count = Math.min(children.length, available)
        if (count < children.length) truncated = true
        for (let index = count - 1; index >= 0; index--) {
            stack.push({ item: children[index], depth })
        }
    }
    pushChildren(roots, 0)
    while (stack.length) {
        if (seen++ >= MAX_TOC_ITEMS || items.length >= MAX_TOC_ITEMS) {
            truncated = true
            break
        }
        const { item, depth } = stack.pop()
        const title = boundedOutlineTitle(item?.label)
        if (title.truncated) truncated = true
        const href = checkedOutlineHref(item?.href)
        if (item?.href && !href) truncated = true
        let childDepth = depth
        if (title.value) {
            const size = encoder.encode(title.value).length
                + (href ? encoder.encode(href).length : 0)
            if (textBytes + size > MAX_TOC_TOTAL_TEXT_BYTES) {
                truncated = true
                break
            }
            const entry = { title: title.value, depth }
            if (href) entry.href = href
            items.push(entry)
            textBytes += size
            childDepth++
        }
        if (childDepth > MAX_TOC_DEPTH) {
            if (item?.subitems?.length) truncated = true
        } else pushChildren(item?.subitems, childDepth)
    }
    return Object.freeze({ items, truncated })
}

const firstTocHref = items => {
    const stack = (items ?? []).slice(0, MAX_TOC_ITEMS).reverse()
    for (let seen = 0; stack.length && seen < MAX_TOC_ITEMS; seen++) {
        const item = stack.pop()
        if (item?.href) return item.href
        const nested = item?.subitems ?? []
        for (let index = nested.length - 1; index >= 0; index--) {
            if (stack.length >= MAX_TOC_ITEMS) break
            stack.push(nested[index])
        }
    }
    return null
}

const initialTargets = book => {
    const targets = []
    const keys = new Set()
    const add = target => {
        if (target === null || target === undefined) return
        const key = `${typeof target}:${target}`
        if (keys.has(key) || targets.length >= MAX_INITIAL_TARGETS) return
        keys.add(key)
        targets.push(target)
    }
    const landmark = book.landmarks?.slice(0, MAX_TOC_ITEMS).find(item =>
        item.type?.includes('bodymatter') || item.type?.includes('text'))
    add(landmark?.href)
    add(firstTocHref(book.toc))
    for (let index = 0; index < book.sections.length
        && targets.length < MAX_INITIAL_TARGETS; index++) {
        if (book.sections[index].linear !== 'no') add(index)
    }
    return targets
}

const nextFrame = () => new Promise(resolve =>
    requestAnimationFrame(() => requestAnimationFrame(resolve)))

const documentHasVisibleText = doc => {
    const walker = doc.createTreeWalker(doc.body, NodeFilter.SHOW_TEXT)
    for (let seen = 0; seen < MAX_TEXT_NODES; seen++) {
        const node = walker.nextNode()
        if (!node) return false
        if (!/\S/u.test(node.nodeValue?.slice(0, MAX_TEXT_SAMPLE) ?? '')) {
            continue
        }
        const range = doc.createRange()
        range.selectNodeContents(node)
        if (range.getClientRects().length) return true
    }
    return false
}

const sectionIsVisible = async view => {
    await nextFrame()
    const contents = view.renderer?.getContents?.() ?? []
    const doc = contents[0]?.doc
    if (!doc?.body) return false
    const images = Array.from(doc.images).slice(0, MAX_MEDIA_CANDIDATES)
    await Promise.allSettled(images.map(image => image.decode()))
    await nextFrame()
    if (documentHasVisibleText(doc)) return true
    if (images.some(image => image.naturalWidth > 0
        && image.getClientRects().length > 0)) return true
    const media = Array.from(doc.body.querySelectorAll('svg, video, audio'))
        .slice(0, MAX_MEDIA_CANDIDATES)
    return media.some(element => element.getClientRects().length > 0)
}

const resolvedTarget = (view, target) => {
    const resolved = view.resolveNavigation(target)
    const index = resolved?.index
    if (!Number.isInteger(index)
        || index < 0 || index >= view.book.sections.length) {
        throw new Error('EPUB target could not be resolved')
    }
    return resolved
}

const showTarget = async (view, target) => {
    const resolved = resolvedTarget(view, target)
    await view.renderer.goTo(resolved)
    if (!await sectionIsVisible(view)) {
        throw new Error('EPUB target has no visible content')
    }
    view.history.pushState(target)
}

const showFirstVisibleSection = async (view, book) => {
    const targets = initialTargets(book)
    for (const target of targets) {
        try {
            await showTarget(view, target)
            return
        } catch (error) {
            console.warn(error)
        }
    }
    throw new Error('EPUB text start has no visible content')
}

const showLocation = async (view, location) => {
    const targets = [
        [location.cfi, true],
        [location.href, false],
    ]
    if (location.fraction !== undefined) {
        targets.push([{ fraction: location.fraction }, false])
    }
    for (const [target, exact] of targets) {
        try {
            await showTarget(view, target)
            const index = view.lastLocation?.section?.current
            if (exact && view.book.sections[index]?.id !== location.href) {
                throw new Error('EPUB location CFI and href do not match')
            }
            if (view.isFixedLayout && location.x !== undefined) {
                view.renderer.setViewport(location.x, location.y)
            }
            return exact
        } catch (error) {
            console.warn(error)
        }
    }
    throw new Error('EPUB location could not be restored')
}

const locatorFromRelocation = (book, relocation) => {
    const index = relocation?.section?.current
    const href = Number.isInteger(index) ? book.sections[index]?.id : null
    const value = {
        cfi: typeof relocation?.cfi === 'string'
            ? collapseCFI(relocation.cfi) : relocation?.cfi,
        href,
        fraction: relocation?.fraction,
        x: relocation?.x,
        y: relocation?.y,
    }
    return checkedLocator(value)
}

const flushLocation = session => {
    if (current !== session || !session.location) return
    if (session.locationTimer) clearTimeout(session.locationTimer)
    session.locationTimer = null
    const user = session.locationUser
    session.locationUser = false
    post('location', { location: session.location, user })
}

const queueLocation = (session, relocation) => {
    if (current !== session) return
    const reason = relocation?.reason
    const userMovement = performance.now()
        <= session.userMovementDeadline
    if (!session.opening
        && !session.commandNavigation
        && reason !== 'navigation'
        && reason !== 'page'
        && reason !== 'snap'
        && !(reason === 'scroll' && userMovement)) return
    try {
        session.location = locatorFromRelocation(session.book, relocation)
        session.locationUser ||= userMovement
    } catch (error) {
        console.warn(error)
        return
    }
    if (session.opening) return
    if (!session.locationTimer) {
        session.locationTimer = setTimeout(
            () => flushLocation(session), LOCATION_DELAY_MS)
    }
}

const sameSelection = (left, right) => left === right
    || left && right
        && left.href === right.href
        && left.start === right.start
        && left.end === right.end

const emitSelection = (session, selection) => {
    if (current !== session || sameSelection(session.selection, selection)) {
        return
    }
    session.selection = selection
    post('selection', { selection })
}

const selectionFromDocument = (session, doc) => {
    const content = session.view.renderer.getContents()
        .find(item => item.doc === doc)
    const selection = doc.getSelection()
    if (!content || !selection || selection.rangeCount !== 1
        || selection.isCollapsed) return null
    const range = selection.getRangeAt(0)
    if (range.startContainer.ownerDocument !== doc
        || range.endContainer.ownerDocument !== doc) return null
    const startRange = range.cloneRange()
    const endRange = range.cloneRange()
    startRange.collapse(true)
    endRange.collapse(false)
    return checkedSelection({
        href: session.book.sections[content.index]?.id,
        start: session.view.getCFI(content.index, startRange),
        end: session.view.getCFI(content.index, endRange),
    })
}

const queueSelection = (session, doc) => {
    if (current !== session) return
    session.selectionDocument = doc
    session.selectionFrame ??= requestAnimationFrame(() => {
        session.selectionFrame = null
        const selectionDoc = session.selectionDocument
        session.selectionDocument = null
        if (current !== session || !selectionDoc) return
        try {
            emitSelection(
                session,
                selectionFromDocument(session, selectionDoc))
        } catch (error) {
            console.warn(error)
            emitSelection(session, null)
        }
    })
}

const cancelPendingSelection = session => {
    session.selectionRevision++
    if (session.pendingNavigation?.command === 'show-selection') {
        session.pendingNavigation = null
    }
}

const installSelectionTracking = (doc, session) => {
    let pointerSelecting = false
    doc.addEventListener('pointerdown', () => {
        cancelPendingSelection(session)
        pointerSelecting = true
    })
    const finishPointerSelection = () => {
        pointerSelecting = false
        queueSelection(session, doc)
    }
    doc.addEventListener('pointerup', finishPointerSelection)
    doc.addEventListener('pointercancel', finishPointerSelection)
    doc.addEventListener('selectionchange', () => {
        if (!pointerSelecting) queueSelection(session, doc)
    })
}

const clearSelection = ({ view: viewID }) => {
    viewID = checkedView(viewID)
    if (!current || current.viewID !== viewID) {
        throw new Error('EPUB view is not open')
    }
    const session = current
    cancelPendingSelection(session)
    session.view.deselect()
    emitSelection(session, null)
}

const selectedRange = (session, selection) => {
    const start = session.view.resolveCFI(selection.start)
    const end = session.view.resolveCFI(selection.end)
    if (!Number.isInteger(start?.index) || start.index !== end?.index
        || session.book.sections[start.index]?.id !== selection.href) {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection range is unavailable')
    }
    const content = session.view.renderer.getContents()
        .find(item => item.index === start.index)
    if (!content?.doc || typeof start.anchor !== 'function'
        || typeof end.anchor !== 'function') {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection range is unavailable')
    }
    const startRange = start.anchor(content.doc)
    const endRange = end.anchor(content.doc)
    if (!startRange?.collapsed || !endRange?.collapsed
        || startRange.startContainer?.ownerDocument !== content.doc
        || endRange.startContainer?.ownerDocument !== content.doc) {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection range is unavailable')
    }
    const range = content.doc.createRange()
    range.setStart(startRange.startContainer, startRange.startOffset)
    range.setEnd(endRange.startContainer, endRange.startOffset)
    if (range.collapsed) {
        selectionTextError(
            'selection-unavailable',
            'EPUB selection range is unavailable')
    }
    return range
}

const applySelection = (session, selection) => {
    const range = selectedRange(session, selection)
    const doc = range.startContainer.ownerDocument
    const selected = doc.getSelection()
    if (!selected) throw new Error('EPUB selection is unavailable')
    selected.removeAllRanges()
    selected.addRange(range)
    const actual = selectionFromDocument(session, doc)
    if (!sameSelection(actual, selection)) {
        selected.removeAllRanges()
        throw new Error('EPUB selection endpoints were not retained')
    }
    emitSelection(session, actual)
}

const selectionText = request => {
    try {
        const { viewID, selection, offset, characterLimit }
            = checkedSelectionTextRequest(request)
        if (!current || current.viewID !== viewID
            || !sameSelection(current.selection, selection)) {
            selectionTextError(
                'selection-no-longer-current',
                'EPUB selection is no longer current')
        }
        const text = selectedRange(current, selection).toString()
        if (text.length > MAX_SELECTION_CHARACTERS * 2) {
            selectionTextError(
                'selection-too-large',
                'EPUB selection exceeds its character limit')
        }
        const characters = Array.from(text)
        if (characters.length > MAX_SELECTION_CHARACTERS) {
            selectionTextError(
                'selection-too-large',
                'EPUB selection exceeds its character limit')
        }
        if (offset > characters.length) {
            selectionTextError(
                'invalid-selection-offset',
                'EPUB selection offset lies outside the selection')
        }
        const nextOffset = Math.min(
            characters.length, offset + characterLimit)
        const done = nextOffset === characters.length
        const result = {
            text: characters.slice(offset, nextOffset).join(''),
            total: characters.length,
            done,
        }
        if (!done) result['next-offset'] = nextOffset
        return { ok: true, result }
    } catch (error) {
        const code = Object.hasOwn(
            SELECTION_TEXT_ERROR_MESSAGES, error?.code)
            ? error.code : 'selection-unavailable'
        if (code === 'selection-unavailable') console.warn(error)
        return {
            ok: false,
            error: { code, message: SELECTION_TEXT_ERROR_MESSAGES[code] },
        }
    }
}

const nextSearchSection = (sections, start) => {
    for (let index = start; index < sections.length; index++) {
        const section = sections[index]
        if (typeof section?.id === 'string'
            && typeof section.createDocument === 'function') {
            return { index, section }
        }
    }
    return null
}

const previousSearchSection = (sections, start) => {
    for (let index = start; index >= 0; index--) {
        const section = sections[index]
        if (typeof section?.id === 'string'
            && typeof section.createDocument === 'function') {
            return { index, section }
        }
    }
    return null
}

const directionalSearchSection = (sections, direction, start) =>
    direction === 'backward'
        ? previousSearchSection(sections, start)
        : nextSearchSection(sections, start)

const searchOriginRange = (session, origin, index, doc) => {
    if (!origin) return null
    const resolved = session.view.resolveCFI(origin.cfi)
    if (!Number.isInteger(resolved?.index) || resolved.index !== index
        || session.book.sections[index]?.id !== origin.href
        || typeof resolved.anchor !== 'function') {
        searchError(
            'invalid-search-cursor',
            'EPUB search origin is invalid')
    }
    const range = resolved.anchor(doc)
    if (!range?.startContainer
        || !Number.isSafeInteger(range.startOffset)) {
        searchError(
            'invalid-search-cursor',
            'EPUB search origin is invalid')
    }
    return range
}

const searchBatch = (matches, cursor, done) => {
    const result = { matches, cursor, done }
    if (done) delete result.cursor
    return result
}

const searchBatchFits = result => encoder.encode(JSON.stringify({
    ok: true, result,
})).length <= MAX_SEARCH_RESULT_BYTES

const searchRequest = async request => {
    try {
        const {
            viewID, query, caseSensitive, direction, origin, cursor,
            matchLimit, sectionLimit,
        } = checkedSearchRequest(request)
        if (!current || current.viewID !== viewID) {
            searchError('search-unavailable', 'EPUB view is not open')
        }
        const session = current
        const sections = session.book.sections
        let entry
        let offset
        if (cursor) {
            const index = sections.findIndex(section =>
                section?.id === cursor.href
                && typeof section.createDocument === 'function')
            if (index < 0) {
                searchError(
                    'invalid-search-cursor',
                    'EPUB search cursor is invalid')
            }
            entry = { index, section: sections[index] }
            offset = cursor.offset
        } else if (origin) {
            const resolved = session.view.resolveCFI(origin.cfi)
            const index = resolved?.index
            if (!Number.isInteger(index)
                || sections[index]?.id !== origin.href
                || typeof sections[index]?.createDocument !== 'function') {
                searchError(
                    'invalid-search-cursor',
                    'EPUB search origin is invalid')
            }
            entry = { index, section: sections[index] }
            offset = null
        } else {
            const start = direction === 'backward'
                ? sections.length - 1 : 0
            entry = directionalSearchSection(sections, direction, start)
            offset = null
        }
        const matcher = searchMatcher(textWalker, {
            defaultLocale: session.book.metadata?.language,
            matchCase: caseSensitive,
            matchDiacritics: true,
            matchWholeWords: false,
        })
        const matches = []
        let sectionsSearched = 0
        while (entry && sectionsSearched < sectionLimit) {
            const { index, section } = entry
            const doc = await section.createDocument()
            if (current !== session) {
                searchError(
                    'search-unavailable',
                    'EPUB search was superseded')
            }
            const originRange = origin && section.id === origin.href
                ? searchOriginRange(session, origin, index, doc) : null
            const found = Array.from(matcher(doc, query))
            if (offset !== null && offset > found.length) {
                searchError(
                    'invalid-search-cursor',
                    'EPUB search cursor is invalid')
            }
            const indexed = found.map((item, ordinal) => ({ item, ordinal }))
            const eligible = indexed.filter(({ item: { range }, ordinal }) => {
                if (offset !== null) {
                    return direction === 'backward'
                        ? ordinal < offset : ordinal >= offset
                }
                if (!originRange) return true
                const relation = originRange.comparePoint(
                    range.startContainer, range.startOffset)
                return direction === 'backward' ? relation < 0 : relation > 0
            })
            if (direction === 'backward') eligible.reverse()
            for (const { item: { range, excerpt }, ordinal } of eligible) {
                const rangeCFI = session.view.getCFI(index, range)
                const selection = checkedSelection({
                    href: section.id,
                    start: collapseCFI(rangeCFI),
                    end: collapseCFI(rangeCFI, true),
                })
                const result = {
                    ...selection,
                    text: range.toString(),
                    before: excerpt.pre,
                    after: excerpt.post,
                }
                const nextCursor = {
                    href: section.id,
                    offset: direction === 'backward' ? ordinal : ordinal + 1,
                }
                if (nextCursor.offset > MAX_SEARCH_CURSOR_OFFSET) {
                    searchError(
                        'search-unavailable',
                        'EPUB search cursor exceeds its limit')
                }
                const candidate = searchBatch(
                    [...matches, result], nextCursor, false)
                if (!searchBatchFits(candidate)) {
                    if (!matches.length) {
                        searchError(
                            'search-result-too-large',
                            'EPUB search result exceeds its byte limit')
                    }
                    return {
                        ok: true,
                        result: searchBatch(
                            matches,
                            {
                                href: section.id,
                                offset: direction === 'backward'
                                    ? ordinal + 1 : ordinal,
                            },
                            false),
                    }
                }
                matches.push(result)
                if (matches.length === matchLimit) {
                    return {
                        ok: true,
                        result: searchBatch(
                            matches, nextCursor, false),
                    }
                }
            }
            offset = null
            sectionsSearched++
            entry = directionalSearchSection(
                sections, direction,
                direction === 'backward' ? index - 1 : index + 1)
        }
        return {
            ok: true,
            result: searchBatch(
                matches,
                entry ? { href: entry.section.id, offset: null } : null,
                !entry),
        }
    } catch (error) {
        const code = Object.hasOwn(SEARCH_ERROR_MESSAGES, error?.code)
            ? error.code : 'search-unavailable'
        if (code === 'search-unavailable') console.warn(error)
        return {
            ok: false,
            error: { code, message: SEARCH_ERROR_MESSAGES[code] },
        }
    }
}

const search = request => {
    const requestID = request?.request
    void searchRequest(request).then(response => {
        if (Number.isSafeInteger(requestID) && requestID > 0) {
            postSearchResult(requestID, response)
        }
    })
}

const open = async ({
    view: viewID, resourceRoot, location, style, zoom, scrollBars,
    rendererAccelerators,
}) => {
    const mine = ++generation
    closeCurrent()
    status.hidden = false
    status.textContent = 'Opening EPUB...'
    try {
        viewID = checkedView(viewID)
        location = location ? checkedLocator(location) : null
        scrollBars = checkedScrollBars(scrollBars)
        checkedRendererAccelerators(rendererAccelerators)
        const root = checkedRoot(resourceRoot)
        const loader = await makeLoader(root)
        const book = await new EPUB(loader).init()
        protectBook(book)
        const view = document.createElement('foliate-view')
        let session
        view.addEventListener('load', event => {
            installReaderCharacterKeys(event.detail.doc)
            installLocationActivity(event.detail.doc, session)
            installSelectionTracking(event.detail.doc, session)
            if (session) emitSelection(session, null)
        })
        view.addEventListener('external-link', event => {
            event.preventDefault()
            try {
                post('external-link', {
                    uri: checkedExternalURI(event.detail?.href),
                })
            } catch (error) {
                console.warn(error)
            }
        })
        await view.open(book)
        if (view.isFixedLayout) {
            if (style != null) {
                throw new Error(
                    'Fixed-layout EPUB cannot use reflow style')
            }
            zoom = checkedZoom(zoom ?? 'fit-page')
        } else {
            if (zoom != null) {
                throw new Error(
                    'Reflowable EPUB cannot use fixed-layout zoom')
            }
            style = checkedStyle(style)
        }
        view.renderer.setAttribute('flow', 'scrolled')
        view.renderer.setAttribute('animated', '')
        applyReadingStyle(view, style)
        applyScrollBars(view, scrollBars)
        Object.assign(view.renderer.style, {
            display: 'block',
            height: '100%',
            width: '100%',
        })
        if (mine !== generation) {
            view.close()
            book.destroy()
            return
        }
        reader.append(view)
        session = {
            generation: mine,
            viewID,
            book,
            view,
            location: null,
            locationUser: false,
            locationTimer: null,
            selection: null,
            selectionDocument: null,
            selectionFrame: null,
            style,
            zoom,
            effectiveScale: null,
            pendingStyle: null,
            styleFrame: null,
            opening: true,
            commandNavigation: false,
            userMovementDeadline: 0,
            pendingNavigation: null,
            navigationRunning: false,
            searchResultRevision: 0,
            selectionRevision: 0,
        }
        installLocationActivity(view, session)
        current = session
        view.renderer.addEventListener('zoom', event => {
            const scale = event.detail?.scale
            if (current === session && Number.isFinite(scale) && scale > 0
                && scale !== session.effectiveScale) {
                session.effectiveScale = scale
                post('zoom-changed', { scale })
            }
        })
        if (view.isFixedLayout) applyFixedZoom(view, zoom)
        view.renderer.addEventListener('relocate', event => {
            queueLocation(session, {
                ...view.lastLocation,
                reason: event.detail.reason,
            })
        })
        view.renderer.addEventListener('boundary-scroll', event => {
            const command = event.detail?.direction > 0
                ? 'next-screen' : 'previous-screen'
            scheduleNavigation(session, { command })
        })
        status.hidden = true
        let restoredLocation = false
        if (location) {
            try {
                restoredLocation = await showLocation(view, location)
            } catch (error) {
                console.warn(error)
                await showFirstVisibleSection(view, book)
            }
        } else await showFirstVisibleSection(view, book)
        if (mine !== generation || current !== session) return
        if (restoredLocation) session.location = location
        session.location ??= locatorFromRelocation(book, view.lastLocation)
        if (session.locationTimer) clearTimeout(session.locationTimer)
        session.locationTimer = null
        session.opening = false
        post('publication-ready', {
            location: session.location,
            outline: outlineFromBook(book.toc),
        })
    } catch (error) {
        if (mine !== generation) return
        closeCurrent()
        status.hidden = false
        status.textContent = 'Could not open EPUB.'
        post('publication-error', { message: error?.message ?? error })
    }
}

const selectionCFI = (session, selection, label) => {
    const start = session.view.resolveNavigation(selection.start)
    const end = session.view.resolveNavigation(selection.end)
    const index = start?.index
    if (!Number.isInteger(index) || end?.index !== index
        || session.book.sections[index]?.id !== selection.href) {
        throw new Error(`EPUB ${label} is not in its spine`)
    }
    return fromRangeEndpoints(selection.start, selection.end)
}

const paintSearchResult = (session, cfi) =>
    session.view.setSearchResult(cfi, {
        drawOptions: { color: SEARCH_HIGHLIGHT_COLOR },
    })

const lineDistance = session => {
    const content = session.view.renderer.getContents()?.[0]
    const computed = content?.doc?.defaultView
        ?.getComputedStyle(content.doc.body)?.lineHeight
    const measured = Number.parseFloat(computed)
    const fallback = 16 * session.style.fontScale * session.style.lineHeight
    return Math.max(8, Math.min(256,
        Number.isFinite(measured) && measured > 0 ? measured : fallback))
}

const turnFixedPage = async (session, direction, end = false) => {
    const moved = direction > 0
        ? await session.view.next() : await session.view.prev()
    if (moved) session.view.renderer.setViewport(0, 0, end)
    return moved
}

const moveFixedViewport = async (session, distance) => {
    if (await session.view.renderer.moveBy(distance)) return true
    return turnFixedPage(session, Math.sign(distance), distance < 0)
}

const showBoundary = async (session, end) => {
    const sections = session.book.sections
    const index = end
        ? sections.findLastIndex(section => section.linear !== 'no')
        : sections.findIndex(section => section.linear !== 'no')
    if (index < 0) throw new Error('EPUB has no linear spine item')
    await session.view.renderer.goTo({ index, anchor: end ? 1 : 0 })
    if (!await sectionIsVisible(session.view)) {
        throw new Error('EPUB boundary has no visible content')
    }
    if (session.view.isFixedLayout) {
        session.view.renderer.setViewport(0, 0, end)
    }
}

const runNavigation = async (session, navigation) => {
    if (current !== session) return
    const { command, location, selection, revision } = navigation
    session.commandNavigation = true
    try {
        switch (command) {
        case 'previous-page':
            if (session.view.isFixedLayout) {
                await turnFixedPage(session, -1)
            } else await session.view.prev()
            break
        case 'next-page':
            if (session.view.isFixedLayout) {
                await turnFixedPage(session, 1)
            } else await session.view.next()
            break
        case 'previous-screen':
            if (session.view.isFixedLayout) {
                await moveFixedViewport(
                    session,
                    -Math.max(1, session.view.renderer.clientHeight / 2))
            } else await session.view.prev()
            break
        case 'next-screen':
            if (session.view.isFixedLayout) {
                await moveFixedViewport(
                    session,
                    Math.max(1, session.view.renderer.clientHeight / 2))
            } else await session.view.next()
            break
        case 'previous-line':
            if (session.view.isFixedLayout) {
                await moveFixedViewport(session, -40)
            }
            else await session.view.prev(lineDistance(session), false)
            break
        case 'next-line':
            if (session.view.isFixedLayout) {
                await moveFixedViewport(session, 40)
            }
            else await session.view.next(lineDistance(session), false)
            break
        case 'first':
            await showBoundary(session, false)
            break
        case 'last':
            await showBoundary(session, true)
            break
        case 'go-to':
            await showLocation(session.view, location)
            break
        case 'show-search-result': {
            const cfi = selectionCFI(session, selection, 'search result')
            await showTarget(session.view, cfi)
            if (current === session
                && revision === session.searchResultRevision) {
                await paintSearchResult(session, cfi)
            }
            break
        }
        case 'show-selection': {
            const cfi = selectionCFI(session, selection, 'selection')
            await showTarget(session.view, cfi)
            if (current === session
                && revision === session.selectionRevision) {
                applySelection(session, selection)
            }
            break
        }
        default:
            throw new Error(`Unsupported EPUB navigation command: ${command}`)
        }
    } finally {
        session.commandNavigation = false
    }
}

const drainNavigation = async session => {
    if (session.navigationRunning) return
    session.navigationRunning = true
    try {
        while (current === session && session.pendingNavigation) {
            const navigation = session.pendingNavigation
            session.pendingNavigation = null
            await runNavigation(session, navigation)
        }
    } catch (error) {
        session.pendingNavigation = null
        if (current === session) {
            post('navigation-error', {
                message: error?.message ?? error,
            })
        }
    } finally {
        session.navigationRunning = false
    }
}

const scheduleNavigation = (session, navigation) => {
    if (current !== session) return
    session.pendingNavigation = navigation
    void drainNavigation(session)
}

const navigate = ({ view: viewID, command, location }) => {
    try {
        viewID = checkedView(viewID)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        if (typeof command !== 'string') {
            throw new Error('Invalid EPUB navigation command')
        }
        location = command === 'go-to'
            ? checkedNavigationTarget(location) : null
        const session = current
        scheduleNavigation(session, { command, location })
    } catch (error) {
        post('navigation-error', { message: error?.message ?? error })
    }
}

const setSearchResult = request => {
    try {
        const { viewID, selection, reveal } =
            checkedSearchResultRequest(request)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        const session = current
        const revision = ++session.searchResultRevision
        session.view.setSearchResult(null)
        if (session.pendingNavigation?.command === 'show-search-result') {
            session.pendingNavigation = null
        }
        if (selection && reveal) {
            session.pendingNavigation = {
                command: 'show-search-result', selection, revision,
            }
            void drainNavigation(session)
        } else if (selection) {
            const cfi = selectionCFI(
                session, selection, 'search result')
            void Promise.resolve(paintSearchResult(session, cfi))
                .catch(error => {
                    if (current === session
                        && revision === session.searchResultRevision) {
                        post('navigation-error', {
                            message: error?.message ?? error,
                        })
                    }
                })
        }
    } catch (error) {
        post('navigation-error', { message: error?.message ?? error })
    }
}

const setSelection = request => {
    try {
        const { viewID, selection } = checkedSetSelectionRequest(request)
        if (!current || current.viewID !== viewID) {
            throw new Error('EPUB view is not open')
        }
        const session = current
        const revision = ++session.selectionRevision
        session.view.deselect()
        session.pendingNavigation = {
            command: 'show-selection', selection, revision,
        }
        void drainNavigation(session)
    } catch (error) {
        post('navigation-error', { message: error?.message ?? error })
    }
}

globalThis.yungeReader = Object.freeze({
    clearSelection, navigate, open, search, selectionText, setScrollBars,
    setSearchResult, setSelection, setStyle, setZoom,
})
