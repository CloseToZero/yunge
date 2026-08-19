// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

const BOOK_ROOT = 'https://yunge-reader-book.localhost/'
const MAX_EXTERNAL_URI_BYTES = 4096
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
    '+', '-', '=', 'G', 'J', 'K', 'SPC', 'g', 'j', 'k', 'y',
])

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

export const checkedRoot = value => {
    const url = new URL(value)
    if (url.origin !== BOOK_ROOT.slice(0, -1)
        || !/^\/[0-9a-f]{32}\/$/u.test(url.pathname)
        || url.search || url.hash || url.username || url.password) {
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
