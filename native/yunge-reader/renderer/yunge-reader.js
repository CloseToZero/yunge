import './foliate-js/view.js'
import { EPUB } from './foliate-js/epub.js'

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
const LOCATION_DELAY_MS = 75
let generation = 0
let current = null

const post = (event, { message, location } = {}) => {
    const payload = { protocol: 1, event }
    if (message) payload.message = String(message).slice(0, 4096)
    if (location) payload.location = location
    window.ipc.postMessage(JSON.stringify(payload))
}

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
    current.view.close()
    current.book.destroy()
    current.view.remove()
    current = null
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
        cfi: relocation?.cfi,
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
    try {
        session.location = locatorFromRelocation(session.book, relocation)
    } catch (error) {
        console.warn(error)
        return
    }
    if (!session.locationTimer) {
        session.locationTimer = setTimeout(
            () => flushLocation(session), LOCATION_DELAY_MS)
    }
}

const open = async ({ view: viewID, resourceRoot, location }) => {
    const mine = ++generation
    closeCurrent()
    status.hidden = false
    status.textContent = 'Opening EPUB...'
    try {
        viewID = checkedView(viewID)
        location = location ? checkedLocator(location) : null
        const root = checkedRoot(resourceRoot)
        const loader = await makeLoader(root)
        const book = await new EPUB(loader).init()
        protectBook(book)
        const view = document.createElement('foliate-view')
        view.addEventListener('external-link', event => {
            event.preventDefault()
        })
        await view.open(book)
        view.renderer.setAttribute('flow', 'scrolled')
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
        const session = {
            generation: mine,
            viewID,
            book,
            view,
            location: null,
            locationTimer: null,
            navigation: Promise.resolve(),
        }
        current = session
        view.addEventListener('relocate', event => {
            queueLocation(session, event.detail)
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
        post('publication-ready', { location: session.location })
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
        location = command === 'go-to' ? checkedLocator(location) : null
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

globalThis.yungeReader = Object.freeze({ navigate, open })
