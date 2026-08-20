// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

const MAX_EXTERNAL_URI_BYTES = 4096
const MAX_INITIAL_TARGETS = 8
const MAX_LOCATOR_TEXT_BYTES = 3072
const MAX_TOC_ITEMS = 4096
const MAX_TOC_DEPTH = 256
const MAX_TOC_HREF_BYTES = 3072
const MAX_TOC_TITLE_BYTES = 1024
const MAX_TOC_TOTAL_TEXT_BYTES = 384 * 1024
const MAX_VIEWPORT_COORDINATE = 1000000
const MIN_FIXED_SCALE = 0.25
const MAX_FIXED_SCALE = 8.0

const APPEARANCE_COLOR_KEYS = Object.freeze([
    'foreground',
    'background',
    'link',
    'selection-foreground',
    'selection-background',
    'search-background',
])

export const READER_CHARACTER_KEYS = Object.freeze([
    "'", '+', '-', '=', '<escape>', '<next>', '<prior>', 'C-d', 'C-g',
    'C-u', 'G', 'J', 'K', 'M-m', 'SPC', 'g', 'j', 'k', 'm', 'y',
])

export const readerKey = event => {
    if (event.defaultPrevented || event.isComposing
        || event.target?.closest?.(
            'input, textarea, select, '
            + '[contenteditable]:not([contenteditable="false"])')) {
        return null
    }
    if (!event.shiftKey && !event.altKey && !event.metaKey
        && event.ctrlKey) {
        const key = `C-${event.key.toLowerCase()}`
        return READER_CHARACTER_KEYS.includes(key) ? key : null
    }
    if (!event.shiftKey && !event.ctrlKey && !event.metaKey
        && event.altKey) {
        return event.code === 'KeyM' ? 'M-m' : null
    }
    if (event.ctrlKey || event.altKey || event.metaKey) return null
    if (event.key === 'Escape') return '<escape>'
    if (event.key === 'PageDown') return '<next>'
    if (event.key === 'PageUp') return '<prior>'
    if (event.code === 'Space') return 'SPC'
    return READER_CHARACTER_KEYS.includes(event.key)
        ? event.key : null
}

export const DEFAULT_STYLE = Object.freeze({
    'font-scale': 1.0,
    'line-height': 1.6,
    'content-width': 720,
    'side-padding': 7.0,
})

const encoder = new TextEncoder()

export const checkedExternalURI = value => {
    if (typeof value !== 'string' || !value
        || encoder.encode(value).length > MAX_EXTERNAL_URI_BYTES
        || /[\s\p{Cc}]/u.test(value)
        || !/^[A-Za-z][A-Za-z0-9+.-]*:/u.test(value)) {
        throw new Error('Invalid EPUB external URI')
    }
    return value
}

export const checkedRendererAccelerators = value => {
    if (!Array.isArray(value)
        || value.length !== READER_CHARACTER_KEYS.length
        || value.some((key, index) =>
            key !== READER_CHARACTER_KEYS[index])) {
        throw new Error('Incompatible EPUB renderer accelerator contract')
    }
    return value
}

export const encodePath = path =>
    path.split('/').map(encodeURIComponent).join('/')

export const checkedRoot = (value, rendererValue) => {
    let url
    let renderer
    try {
        url = new URL(value)
        renderer = new URL(rendererValue)
    } catch {
        throw new Error('Invalid Yunge Reader publication resource root')
    }
    const resource = url.pathname.match(
        /^\/([0-9a-f]{32})\/book\/[0-9a-f]{32}\/$/u)
    const rendererPath = renderer.pathname.match(
        /^\/([0-9a-f]{32})\/app\/index\.html$/u)
    if (url.protocol !== 'http:'
        || url.hostname !== '127.0.0.1'
        || !url.port
        || url.origin !== renderer.origin
        || !resource || !rendererPath
        || resource[1] !== rendererPath[1]
        || url.search || url.hash || url.username || url.password
        || renderer.search || renderer.hash
        || renderer.username || renderer.password) {
        throw new Error('Invalid Yunge Reader publication resource root')
    }
    return url.href
}

export const checkedView = value => {
    if (!Number.isSafeInteger(value) || value < 1) {
        throw new Error('Invalid Yunge Reader view identifier')
    }
    return value
}

export const checkedLocatorText = (value, name) => {
    if (typeof value !== 'string' || !value
        || encoder.encode(value).length > MAX_LOCATOR_TEXT_BYTES
        || /[\u0000-\u001f\u007f]/u.test(value)) {
        throw new Error(`Invalid EPUB locator ${name}`)
    }
    return value
}

export const checkedOutlineHref = value => {
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

export const checkedLocator = value => {
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
        || href.split('/').some(part =>
            !part || ['.', '..'].includes(part))) {
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

export const checkedSelection = value => {
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
    if (!checkedOutlineHref(href) || href.includes('#') || start === end) {
        throw new Error('Invalid EPUB selection range')
    }
    return Object.freeze({ href, start, end })
}

export const checkedNavigationTarget = value => {
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

export const outlineFromBook = toc => {
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

export const initialTargets = book => {
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

export const sameSelection = (left, right) => left === right
    || left && right
        && left.href === right.href
        && left.start === right.start
        && left.end === right.end

export const checkedAppearance = value => {
    const keys = value && typeof value === 'object'
        && !Array.isArray(value) ? Object.keys(value) : []
    if (!value || typeof value !== 'object' || Array.isArray(value)
        || typeof value.mode !== 'string') {
        throw new Error('Invalid EPUB appearance')
    }
    if (value.mode === 'original') {
        if (keys.length !== 1 || keys[0] !== 'mode') {
            throw new Error('Invalid EPUB appearance')
        }
        return Object.freeze({ mode: 'original' })
    }
    if (value.mode !== 'follow-emacs'
        || keys.length !== APPEARANCE_COLOR_KEYS.length + 1
        || keys.some(key => key !== 'mode'
            && !APPEARANCE_COLOR_KEYS.includes(key))
        || APPEARANCE_COLOR_KEYS.some(key =>
            typeof value[key] !== 'string'
            || !/^#[0-9a-f]{6}$/u.test(value[key]))) {
        throw new Error('Invalid EPUB appearance')
    }
    return Object.freeze(Object.fromEntries([
        ['mode', value.mode],
        ...APPEARANCE_COLOR_KEYS.map(key => [key, value[key]]),
    ]))
}

export const checkedStyle = value => {
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

export const readingStyleCSS = style => `
    body {
        font-size: ${style.fontScale}em !important;
        line-height: ${style.lineHeight} !important;
    }
    p, li, blockquote, dd {
        line-height: ${style.lineHeight} !important;
    }
`

export const colorScheme = background => {
    const channels = background.slice(1).match(/../gu)
        .map(channel => Number.parseInt(channel, 16) / 255)
    const linear = channels.map(channel => channel <= 0.04045
        ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4)
    const luminance = linear[0] * 0.2126
        + linear[1] * 0.7152 + linear[2] * 0.0722
    return luminance < 0.18 ? 'dark' : 'light'
}

export const appearanceStyleCSS = appearance => {
    if (appearance.mode === 'original') return ['', '']
    const foreground = appearance.foreground
    const background = appearance.background
    const link = appearance.link
    const selectionForeground = appearance['selection-foreground']
    const selectionBackground = appearance['selection-background']
    return [`
        html {
            color-scheme: ${colorScheme(background)};
            color: ${foreground};
            background-color: ${background};
        }
        a:any-link {
            color: ${link};
        }
    `, `
        html, body {
            color: ${foreground} !important;
            background: ${background} !important;
        }
        body * {
            color: inherit !important;
            border-color: currentcolor !important;
            background-color: transparent !important;
        }
        a:any-link {
            color: ${link} !important;
        }
        ::selection {
            color: ${selectionForeground} !important;
            background: ${selectionBackground} !important;
        }
        img, svg, video, canvas {
            background-color: transparent !important;
        }
    `]
}

export const checkedZoom = value => {
    if (value === 'fit-width' || value === 'fit-page') return value
    if (!Number.isFinite(value)
        || value < MIN_FIXED_SCALE || value > MAX_FIXED_SCALE) {
        throw new Error('Invalid EPUB fixed-layout zoom')
    }
    return value
}

export const checkedScrollBars = value => {
    if (typeof value !== 'boolean') {
        throw new Error('Invalid EPUB scroll bar visibility')
    }
    return value
}

export const sameReadingStyle = (left, right) => left && right
    && left.fontScale === right.fontScale
    && left.lineHeight === right.lineHeight
    && left.contentWidth === right.contentWidth
    && left.sidePadding === right.sidePadding

export const sameAppearance = (left, right) => left && right
    && left.mode === right.mode
    && (left.mode === 'original'
        || APPEARANCE_COLOR_KEYS.every(key => left[key] === right[key]))

export const initialNavigationState = () => Object.freeze({
    running: false,
    pending: null,
})

const navigationTransition = (state, navigation = null) => Object.freeze({
    state: Object.freeze(state),
    navigation,
})

export const reduceNavigation = (state, event) => {
    if (!state || typeof state !== 'object'
        || typeof state.running !== 'boolean'
        || (state.pending !== null
            && typeof state.pending !== 'object')) {
        throw new Error('Invalid EPUB navigation state')
    }
    if (!event || typeof event !== 'object'
        || typeof event.type !== 'string') {
        throw new Error('Invalid EPUB navigation event')
    }
    switch (event.type) {
    case 'enqueue':
        if (!event.navigation || typeof event.navigation !== 'object') {
            throw new Error('Invalid EPUB navigation request')
        }
        return state.running
            ? navigationTransition({
                running: true,
                pending: event.navigation,
            })
            : navigationTransition({
                running: true,
                pending: null,
            }, event.navigation)
    case 'complete':
        if (!state.running) {
            throw new Error('Cannot complete idle EPUB navigation')
        }
        return state.pending
            ? navigationTransition({
                running: true,
                pending: null,
            }, state.pending)
            : navigationTransition(initialNavigationState())
    case 'fail':
    case 'reset':
        return navigationTransition(initialNavigationState())
    case 'cancel-pending':
        if (typeof event.command !== 'string') {
            throw new Error('Invalid EPUB navigation cancellation')
        }
        return navigationTransition({
            running: state.running,
            pending: state.pending?.command === event.command
                ? null : state.pending,
        })
    default:
        throw new Error(`Unsupported EPUB navigation event: ${event.type}`)
    }
}
