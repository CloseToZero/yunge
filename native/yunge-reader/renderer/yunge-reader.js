import './foliate-js/view.js'
import { EPUB } from './foliate-js/epub.js'
import { collapse as collapseCFI } from './foliate-js/epubcfi.js'

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
const MAX_MEDIA_CANDIDATES = 256
const MAX_TEXT_NODES = 4096
const MAX_TEXT_SAMPLE = 4096
const MAX_TOC_ITEMS = 4096
const MAX_TOC_DEPTH = 256
const MAX_TOC_HREF_BYTES = 3072
const MAX_TOC_TITLE_BYTES = 1024
const MAX_TOC_TOTAL_TEXT_BYTES = 384 * 1024
const LOCATION_DELAY_MS = 75
const USER_MOVEMENT_WINDOW_MS = 1000
const DEFAULT_STYLE = Object.freeze({
    'font-scale': 1.0,
    'line-height': 1.6,
    'content-width': 720,
    'side-padding': 7.0,
})
let generation = 0
let current = null

const post = (event, { message, location, outline, key } = {}) => {
    const payload = { protocol: 1, event }
    if (message) payload.message = String(message).slice(0, 4096)
    if (location) payload.location = location
    if (outline) payload.outline = outline
    if (key) payload.key = key
    window.ipc.postMessage(JSON.stringify(payload))
}

const readerCharacterKey = event => {
    if (event.defaultPrevented || event.isComposing
        || event.ctrlKey || event.altKey || event.metaKey
        || event.target?.closest?.(
            'input, textarea, select, '
            + '[contenteditable]:not([contenteditable="false"])')) {
        return null
    }
    return ['J', 'K', '+', '-', '='].includes(event.key)
        ? event.key : null
}

const relayReaderCharacterKey = event => {
    const key = readerCharacterKey(event)
    if (!key) return
    event.preventDefault()
    event.stopImmediatePropagation()
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
        const session = current
        session.pendingStyle = style
        session.styleFrame ??= requestAnimationFrame(
            () => applyPendingStyle(session))
    } catch (error) {
        post('style-error', { message: error?.message ?? error })
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
    if (keys.some(key => !['cfi', 'href', 'fraction'].includes(key))) {
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
    return Object.freeze(result)
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
    if (current.styleFrame !== null) {
        cancelAnimationFrame(current.styleFrame)
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
    const targets = [location.cfi, location.href]
    if (location.fraction !== undefined) {
        targets.push({ fraction: location.fraction })
    }
    for (const target of targets) {
        try {
            await showTarget(view, target)
            return
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
    }
    return checkedLocator(value)
}

const flushLocation = session => {
    if (current !== session || !session.location) return
    if (session.locationTimer) clearTimeout(session.locationTimer)
    session.locationTimer = null
    post('location', { location: session.location })
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

const open = async ({ view: viewID, resourceRoot, location, style }) => {
    const mine = ++generation
    closeCurrent()
    status.hidden = false
    status.textContent = 'Opening EPUB...'
    try {
        viewID = checkedView(viewID)
        location = location ? checkedLocator(location) : null
        style = checkedStyle(style)
        const root = checkedRoot(resourceRoot)
        const loader = await makeLoader(root)
        const book = await new EPUB(loader).init()
        protectBook(book)
        const view = document.createElement('foliate-view')
        let session
        view.addEventListener('load', event => {
            installReaderCharacterKeys(event.detail.doc)
            installLocationActivity(event.detail.doc, session)
        })
        view.addEventListener('external-link', event => {
            event.preventDefault()
        })
        await view.open(book)
        view.renderer.setAttribute('flow', 'scrolled')
        applyReadingStyle(view, style)
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
            locationTimer: null,
            style,
            pendingStyle: null,
            styleFrame: null,
            opening: true,
            commandNavigation: false,
            userMovementDeadline: 0,
            navigation: Promise.resolve(),
        }
        installLocationActivity(view, session)
        current = session
        view.renderer.addEventListener('relocate', event => {
            queueLocation(session, {
                ...view.lastLocation,
                reason: event.detail.reason,
            })
        })
        status.hidden = true
        if (location) {
            try {
                await showLocation(view, location)
            } catch (error) {
                console.warn(error)
                await showFirstVisibleSection(view, book)
            }
        } else await showFirstVisibleSection(view, book)
        if (mine !== generation || current !== session) return
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

const runNavigation = async (session, command, location) => {
    if (current !== session) return
    session.commandNavigation = true
    try {
        switch (command) {
        case 'previous-screen':
            await session.view.prev()
            break
        case 'next-screen':
            await session.view.next()
            break
        case 'go-to':
            await showLocation(session.view, location)
            break
        default:
            throw new Error(`Unsupported EPUB navigation command: ${command}`)
        }
    } finally {
        session.commandNavigation = false
    }
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
        session.navigation = session.navigation
            .then(() => runNavigation(session, command, location))
            .catch(error => {
                if (current === session) {
                    post('navigation-error', {
                        message: error?.message ?? error,
                    })
                }
            })
    } catch (error) {
        post('navigation-error', { message: error?.message ?? error })
    }
}

globalThis.yungeReader = Object.freeze({ navigate, open, setStyle })
