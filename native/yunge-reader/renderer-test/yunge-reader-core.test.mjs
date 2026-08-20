// SPDX-FileCopyrightText: 2026 Chen Zhexuan
// SPDX-License-Identifier: MIT

import assert from 'node:assert/strict'
import test from 'node:test'

import {
    appearanceStyleCSS,
    checkedAppearance,
    checkedExternalURI,
    checkedLocator,
    checkedLocatorText,
    checkedNavigationTarget,
    checkedOutlineHref,
    checkedRendererAccelerators,
    checkedRoot,
    checkedScrollBars,
    checkedSelection,
    checkedStyle,
    checkedView,
    checkedZoom,
    colorScheme,
    encodePath,
    initialTargets,
    initialNavigationState,
    outlineFromBook,
    readerKey,
    READER_CHARACTER_KEYS,
    readingStyleCSS,
    reduceNavigation,
    sameAppearance,
    sameReadingStyle,
    sameSelection,
} from '../renderer/yunge-reader-core.mjs'

const followAppearance = () => ({
    mode: 'follow-emacs',
    foreground: '#112233',
    background: '#fefefe',
    link: '#224466',
    'selection-foreground': '#000000',
    'selection-background': '#ffeeaa',
    'search-background': '#ff7800',
})

const keyEvent = overrides => ({
    defaultPrevented: false,
    isComposing: false,
    shiftKey: false,
    ctrlKey: false,
    altKey: false,
    metaKey: false,
    code: 'KeyA',
    key: 'a',
    target: null,
    ...overrides,
})

test('validates renderer accelerator and path contracts', () => {
    const accelerators = [...READER_CHARACTER_KEYS]
    assert.equal(checkedRendererAccelerators(accelerators), accelerators)
    assert.throws(() => checkedRendererAccelerators(
        [...accelerators].reverse()))
    assert.throws(() => checkedRendererAccelerators(
        [...accelerators, 'x']))
    assert.equal(
        encodePath('OPS/中文 text.xhtml'),
        'OPS/%E4%B8%AD%E6%96%87%20text.xhtml')
})

test('normalizes reader keyboard accelerators', () => {
    assert.equal(readerKey(keyEvent({ ctrlKey: true, key: 'D' })), 'C-d')
    assert.equal(readerKey(keyEvent({ altKey: true,
        code: 'KeyM', key: 'µ' })), 'M-m')
    assert.equal(readerKey(keyEvent({ key: 'Escape' })), '<escape>')
    assert.equal(readerKey(keyEvent({ key: 'PageDown' })), '<next>')
    assert.equal(readerKey(keyEvent({ key: 'PageUp' })), '<prior>')
    assert.equal(readerKey(keyEvent({ code: 'Space', key: ' ' })), 'SPC')
    assert.equal(readerKey(keyEvent({ key: 'j' })), 'j')
    assert.equal(readerKey(keyEvent({ code: 'KeyM', key: 'm' })), 'm')
    assert.equal(readerKey(keyEvent({ code: 'Quote', key: "'" })), "'")

    assert.equal(readerKey(keyEvent({ ctrlKey: true, key: 'x' })), null)
    assert.equal(readerKey(keyEvent({ metaKey: true, key: 'j' })), null)
    assert.equal(readerKey(keyEvent({ defaultPrevented: true })), null)
    assert.equal(readerKey(keyEvent({ isComposing: true })), null)
    assert.equal(readerKey(keyEvent({
        target: { closest: () => ({}) },
    })), null)
})

test('validates publication roots, view identifiers, and external URIs', () => {
    const root = `https://yunge-reader-book.localhost/${'a'.repeat(32)}/`
    assert.equal(checkedRoot(root), root)
    const wkRoot = `yunge-reader-book://localhost/${'b'.repeat(32)}/`
    assert.equal(checkedRoot(wkRoot), wkRoot)
    for (const invalid of [
        `${root}?query`,
        `${root}#fragment`,
        `https://example.test/${'a'.repeat(32)}/`,
        `https://yunge-reader-book.localhost/${'A'.repeat(32)}/`,
    ]) assert.throws(() => checkedRoot(invalid))

    assert.equal(checkedView(1), 1)
    assert.throws(() => checkedView(0))
    assert.throws(() => checkedView(1.5))

    const uri = 'https://example.test/reference?q=reader'
    assert.equal(checkedExternalURI(uri), uri)
    for (const invalid of [
        '/relative',
        'https://example.test/has space',
        `https:${'a'.repeat(4096)}`,
        'https:\u0000example.test',
    ]) assert.throws(() => checkedExternalURI(invalid))
})

test('validates persistent EPUB locations and transient selections', () => {
    const locator = {
        cfi: 'epubcfi(/6/4!/4/2/1:7)',
        href: 'OPS/chapter.xhtml',
        fraction: 0.5,
        x: 12,
        y: 34,
    }
    assert.deepEqual(checkedLocator(locator), locator)
    assert.ok(Object.isFrozen(checkedLocator(locator)))
    assert.deepEqual(
        checkedNavigationTarget({ href: 'OPS/chapter.xhtml#part' }),
        { href: 'OPS/chapter.xhtml#part' })
    assert.equal(
        checkedOutlineHref('OPS/chapter.xhtml#part'),
        'OPS/chapter.xhtml#part')

    const selection = {
        href: 'OPS/chapter.xhtml',
        start: 'epubcfi(/6/4!/4/2/1:7)',
        end: 'epubcfi(/6/4!/4/2/1:11)',
    }
    assert.deepEqual(checkedSelection(selection), selection)
    assert.ok(sameSelection(selection, { ...selection }))
    assert.ok(!sameSelection(selection, { ...selection, end: 'epubcfi(/8)' }))

    for (const invalid of [
        { ...locator, href: '../chapter.xhtml' },
        { ...locator, x: 12, y: undefined },
        { ...locator, fraction: 1.01 },
        { ...locator, extra: true },
    ]) assert.throws(() => checkedLocator(invalid))
    assert.throws(() => checkedSelection({ ...selection,
        end: selection.start }))
    assert.throws(() => checkedSelection({ ...selection,
        href: 'OPS/chapter.xhtml#part' }))
    assert.equal(checkedOutlineHref('https://example.test/chapter'), null)
    assert.throws(() => checkedLocatorText('x'.repeat(3073), 'href'))
})

test('flattens and bounds publication outlines', () => {
    const outline = outlineFromBook([
        {
            label: '  Part\n One  ',
            href: 'OPS/part.xhtml',
            subitems: [{
                label: 'Chapter',
                href: 'OPS/chapter.xhtml#start',
            }],
        },
        { label: 'Unsafe', href: '../outside.xhtml' },
    ])
    assert.deepEqual(outline.items, [
        { title: 'Part One', depth: 0, href: 'OPS/part.xhtml' },
        {
            title: 'Chapter',
            depth: 1,
            href: 'OPS/chapter.xhtml#start',
        },
        { title: 'Unsafe', depth: 0 },
    ])
    assert.equal(outline.truncated, true)

    const longTitle = outlineFromBook([{
        label: '界'.repeat(400),
        href: 'OPS/chapter.xhtml',
    }])
    assert.equal(longTitle.truncated, true)
    assert.ok(Buffer.byteLength(longTitle.items[0].title) <= 1024)

    const oversized = outlineFromBook(Array.from(
        { length: 4097 }, (_, index) => ({
            label: `Chapter ${index}`,
            href: `OPS/${index}.xhtml`,
        })))
    assert.equal(oversized.items.length, 4096)
    assert.equal(oversized.truncated, true)
})

test('chooses a bounded deduplicated set of initial reading targets', () => {
    const targets = initialTargets({
        landmarks: [
            { type: ['cover'], href: 'OPS/cover.xhtml' },
            { type: ['bodymatter'], href: 'OPS/start.xhtml' },
        ],
        toc: [{ label: 'Start', href: 'OPS/start.xhtml' }],
        sections: Array.from({ length: 20 }, (_, index) => ({
            linear: index === 0 ? 'no' : 'yes',
        })),
    })
    assert.deepEqual(targets, [
        'OPS/start.xhtml', 1, 2, 3, 4, 5, 6, 7,
    ])
})

test('normalizes bounded EPUB appearance and style values', () => {
    const original = checkedAppearance({ mode: 'original' })
    assert.deepEqual(original, { mode: 'original' })
    assert.ok(Object.isFrozen(original))
    assert.throws(() => checkedAppearance({
        mode: 'original',
        foreground: '#112233',
    }))

    const source = followAppearance()
    const appearance = checkedAppearance(source)
    assert.deepEqual(appearance, source)
    assert.notEqual(appearance, source)
    assert.ok(Object.isFrozen(appearance))
    assert.throws(() => checkedAppearance({
        ...source,
        foreground: '#ABCDEF',
    }))

    const style = checkedStyle(null)
    assert.deepEqual(style, {
        fontScale: 1,
        lineHeight: 1.6,
        contentWidth: 720,
        sidePadding: 7,
    })
    assert.ok(Object.isFrozen(style))
    assert.throws(() => checkedStyle({
        'font-scale': 1,
        'line-height': 1.6,
        'content-width': 319,
        'side-padding': 7,
    }))
})

test('validates fixed zoom and scroll bar values', () => {
    for (const zoom of ['fit-page', 'fit-width', 0.25, 1, 8]) {
        assert.equal(checkedZoom(zoom), zoom)
    }
    for (const zoom of ['fit-height', 0.24, 8.01, Number.NaN]) {
        assert.throws(() => checkedZoom(zoom))
    }
    assert.equal(checkedScrollBars(true), true)
    assert.equal(checkedScrollBars(false), false)
    assert.throws(() => checkedScrollBars('hidden'))
})

test('derives stable reading styles and appearance CSS', () => {
    const original = checkedAppearance({ mode: 'original' })
    const appearance = checkedAppearance(followAppearance())
    const style = checkedStyle(null)
    assert.equal(colorScheme('#000000'), 'dark')
    assert.equal(colorScheme('#ffffff'), 'light')
    assert.deepEqual(appearanceStyleCSS(original), ['', ''])
    const [before, content] = appearanceStyleCSS(appearance)
    assert.match(before, /color-scheme: light/u)
    assert.match(content, /::selection/u)
    assert.match(content, /background: #fefefe !important/u)
    assert.match(readingStyleCSS(style), /font-size: 1em !important/u)
    assert.ok(sameAppearance(appearance, { ...appearance }))
    assert.ok(!sameAppearance(appearance, {
        ...appearance,
        link: '#ffffff',
    }))
    assert.ok(sameReadingStyle(style, { ...style }))
    assert.ok(!sameReadingStyle(style, {
        ...style,
        lineHeight: 2,
    }))
})

test('coalesces navigation while preserving the active request', () => {
    const first = { command: 'next-page' }
    const second = { command: 'next-line' }
    const latest = { command: 'go-to', location: { href: 'chapter.xhtml' } }
    let transition = reduceNavigation(initialNavigationState(), {
        type: 'enqueue',
        navigation: first,
    })
    assert.deepEqual(transition.navigation, first)
    assert.deepEqual(transition.state, { running: true, pending: null })
    assert.ok(Object.isFrozen(transition.state))

    transition = reduceNavigation(transition.state, {
        type: 'enqueue',
        navigation: second,
    })
    assert.equal(transition.navigation, null)
    assert.deepEqual(transition.state.pending, second)

    transition = reduceNavigation(transition.state, {
        type: 'enqueue',
        navigation: latest,
    })
    assert.equal(transition.navigation, null)
    assert.deepEqual(transition.state.pending, latest)

    transition = reduceNavigation(transition.state, { type: 'complete' })
    assert.deepEqual(transition.navigation, latest)
    assert.deepEqual(transition.state, { running: true, pending: null })
    transition = reduceNavigation(transition.state, { type: 'complete' })
    assert.equal(transition.navigation, null)
    assert.deepEqual(transition.state, initialNavigationState())
})

test('cancels matching pending navigation and drops work on failure', () => {
    let transition = reduceNavigation(initialNavigationState(), {
        type: 'enqueue',
        navigation: { command: 'next-page' },
    })
    transition = reduceNavigation(transition.state, {
        type: 'enqueue',
        navigation: { command: 'show-selection', revision: 4 },
    })
    const pending = transition.state.pending
    transition = reduceNavigation(transition.state, {
        type: 'cancel-pending',
        command: 'show-search-result',
    })
    assert.equal(transition.state.pending, pending)
    transition = reduceNavigation(transition.state, {
        type: 'cancel-pending',
        command: 'show-selection',
    })
    assert.equal(transition.state.pending, null)

    transition = reduceNavigation(transition.state, {
        type: 'enqueue',
        navigation: { command: 'last' },
    })
    assert.equal(transition.state.pending.command, 'last')
    transition = reduceNavigation(transition.state, { type: 'fail' })
    assert.deepEqual(transition.state, initialNavigationState())
    assert.equal(transition.navigation, null)
})

test('bounds an arbitrary navigation burst to active and latest work', () => {
    let state = initialNavigationState()
    const started = []
    for (let index = 0; index < 1000; index++) {
        const transition = reduceNavigation(state, {
            type: 'enqueue',
            navigation: { command: 'go-to', location: index },
        })
        state = transition.state
        if (transition.navigation) started.push(transition.navigation)
    }
    assert.deepEqual(started, [{ command: 'go-to', location: 0 }])
    let transition = reduceNavigation(state, { type: 'complete' })
    assert.deepEqual(
        transition.navigation,
        { command: 'go-to', location: 999 })
    transition = reduceNavigation(transition.state, { type: 'complete' })
    assert.deepEqual(transition.state, initialNavigationState())
})

test('rejects invalid navigation state transitions', () => {
    assert.throws(() => reduceNavigation(null, { type: 'reset' }))
    assert.throws(() => reduceNavigation(
        initialNavigationState(), { type: 'enqueue' }))
    assert.throws(() => reduceNavigation(
        initialNavigationState(), { type: 'complete' }))
    assert.throws(() => reduceNavigation(
        initialNavigationState(), { type: 'unknown' }))
})
