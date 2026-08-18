# SPDX-FileCopyrightText: 2026 Chen Zhexuan
# SPDX-License-Identifier: MIT

param(
    [string]$Emacs
)

$ErrorActionPreference = 'Stop'

function Invoke-Checked {
    param(
        [string]$Program,
        [string[]]$CommandArguments
    )

    & $Program @CommandArguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Program exited with status $LASTEXITCODE"
    }
}

function Stop-IsolatedEmacs {
    param(
        [System.Diagnostics.Process]$Process,
        [string]$Helper
    )

    $children = Get-CimInstance Win32_Process |
        Where-Object {
            $_.ParentProcessId -eq $Process.Id -and
            $_.ExecutablePath -and
            [IO.Path]::GetFullPath($_.ExecutablePath) -eq $Helper
        }
    foreach ($child in $children) {
        Stop-Process -Id $child.ProcessId -ErrorAction SilentlyContinue
    }
    if (-not $Process.HasExited) {
        Stop-Process -Id $Process.Id -ErrorAction SilentlyContinue
    }
}

$repoCandidate = Join-Path $PSScriptRoot '..'
$repoRoot = [IO.Path]::GetFullPath($repoCandidate)
$manifest = Join-Path $repoRoot 'native/yunge-reader/Cargo.toml'
$helperRelative =
    'native/yunge-reader/target/release/yunge-reader.exe'
$helperCandidate = Join-Path $repoRoot $helperRelative
$helper = [IO.Path]::GetFullPath($helperCandidate)
$runnerRelative = 'yunge-reader-fixed-epub-smoke.el'
$runnerCandidate = Join-Path $PSScriptRoot $runnerRelative
$runner = [IO.Path]::GetFullPath($runnerCandidate)
$cargo = (Get-Command cargo).Source
if (-not $Emacs) {
    $Emacs = (Get-Command emacs).Source
}
$Emacs = [IO.Path]::GetFullPath($Emacs)

Invoke-Checked $cargo @(
    'build'
    '--release'
    '--manifest-path'
    $manifest
)
if (-not (Test-Path -LiteralPath $helper -PathType Leaf)) {
    throw "Native helper was not built: $helper"
}

$tempBase = [IO.Path]::GetFullPath([IO.Path]::GetTempPath())
$smokeName = 'yunge-reader-fixed-' + [guid]::NewGuid().ToString('N')
$smokeRoot = [IO.Path]::GetFullPath((Join-Path $tempBase $smokeName))
$pathComparison = [StringComparison]::OrdinalIgnoreCase
if (-not $smokeRoot.StartsWith($tempBase, $pathComparison)) {
    throw 'Smoke directory escaped the system temporary directory'
}
New-Item -ItemType Directory -Path $smokeRoot | Out-Null

$environmentNames = @(
    'YUNGE_READER_ROOT'
    'YUNGE_READER_NATIVE_PROGRAM'
    'YUNGE_READER_FIXED_VARIANT'
    'YUNGE_READER_FIXED_FIXTURE'
    'YUNGE_READER_FIXED_RESULT'
    'YUNGE_READER_FIXED_ELN_CACHE'
)
$savedEnvironment = @{}
foreach ($name in $environmentNames) {
    $savedEnvironment[$name] =
        [Environment]::GetEnvironmentVariable($name, 'Process')
}

try {
    Set-Item Env:YUNGE_READER_ROOT $repoRoot
    Set-Item Env:YUNGE_READER_NATIVE_PROGRAM $helper
    Set-Item Env:YUNGE_READER_FIXED_ELN_CACHE `
        (Join-Path $smokeRoot 'eln-cache')
    foreach ($variant in @('ltr', 'rtl', 'vertical-rl')) {
        $fixture = Join-Path $smokeRoot "$variant.epub"
        $result = Join-Path $smokeRoot "$variant.out"
        Invoke-Checked $cargo @(
            'run'
            '--quiet'
            '--manifest-path'
            $manifest
            '--example'
            'fixed_epub_fixture'
            '--'
            '--variant'
            $variant
            $fixture
        )
        Set-Item Env:YUNGE_READER_FIXED_VARIANT $variant
        Set-Item Env:YUNGE_READER_FIXED_FIXTURE $fixture
        Set-Item Env:YUNGE_READER_FIXED_RESULT $result

        $quotedRunner = '"' + $runner + '"'
        $process = Start-Process `
            -FilePath $Emacs `
            -ArgumentList @('-Q', '--load', $quotedRunner) `
            -WindowStyle Hidden `
            -PassThru
        if (-not $process.WaitForExit(45000)) {
            Stop-IsolatedEmacs $process $helper
            throw "Fixed EPUB smoke timed out for $variant"
        }
        if ($process.ExitCode -ne 0) {
            throw "Isolated Emacs failed for $variant"
        }
        if (-not (Test-Path -LiteralPath $result -PathType Leaf)) {
            throw "Fixed EPUB smoke returned no result for $variant"
        }
        $value = Get-Content -LiteralPath $result -Raw
        if ($value -notmatch ':value passed' -or
            $value -notmatch ':error nil' -or
            $value -notmatch ':warnings nil') {
            throw "Fixed EPUB smoke failed for ${variant}: $value"
        }
        Write-Output "Fixed EPUB smoke passed: $variant"
    }
}
finally {
    foreach ($name in $environmentNames) {
        $value = $savedEnvironment[$name]
        if ($null -eq $value) {
            Remove-Item "Env:$name" -ErrorAction SilentlyContinue
        }
        else {
            Set-Item "Env:$name" $value
        }
    }
    $resolvedSmoke = [IO.Path]::GetFullPath($smokeRoot)
    if (-not $resolvedSmoke.StartsWith($tempBase, $pathComparison)) {
        throw 'Refusing to remove an unexpected smoke directory'
    }
    if (Test-Path -LiteralPath $resolvedSmoke) {
        Remove-Item -LiteralPath $resolvedSmoke -Recurse
    }
}
