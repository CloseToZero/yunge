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
const MAX_INITIAL_TARGETS = 8
const MAX_MEDIA_CANDIDATES = 256
const MAX_TEXT_NODES = 4096
const MAX_TEXT_SAMPLE = 4096
const MAX_TOC_ITEMS = 4096
let generation = 0
let current = null

const post = (event, message) => {
    const payload = { protocol: 1, event }
    if (message) payload.message = String(message).slice(0, 4096)
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

const showFirstVisibleSection = async (view, book) => {
    const targets = initialTargets(book)
    for (const target of targets) {
        const resolved = view.resolveNavigation(target)
        if (!resolved) continue
        try {
            await view.renderer.goTo(resolved)
            if (await sectionIsVisible(view)) {
                view.history.pushState(target)
                return
            }
        } catch (error) {
            console.warn(error)
        }
    }
    throw new Error('EPUB text start has no visible content')
}

const open = async ({ resourceRoot }) => {
    const mine = ++generation
    closeCurrent()
    status.hidden = false
    status.textContent = 'Opening EPUB...'
    try {
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
        current = { book, view }
        status.hidden = true
        await showFirstVisibleSection(view, book)
        post('publication-ready')
    } catch (error) {
        if (mine !== generation) return
        closeCurrent()
        status.hidden = false
        status.textContent = 'Could not open EPUB.'
        post('publication-error', error?.message ?? error)
    }
}

globalThis.yungeReader = Object.freeze({ open })
