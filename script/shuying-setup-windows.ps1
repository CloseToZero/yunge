# SPDX-FileCopyrightText: 2026 Chen Zhexuan
# SPDX-License-Identifier: MIT

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $WorkDirectory,

    [Parameter(Mandatory = $true)]
    [string] $WorkRoot,

    [Parameter(Mandatory = $true)]
    [uri] $DownloadPage
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

# Emacs decodes this process pipe as UTF-8.  Windows PowerShell 5.1 otherwise
# inherits a legacy console code page, so localized output from native tools
# can arrive as undecodable bytes.
$utf8 = New-Object System.Text.UTF8Encoding($false)
[Console]::InputEncoding = $utf8
[Console]::OutputEncoding = $utf8
$OutputEncoding = $utf8

$stage = 'initialization'
$exitCode = 0
$safeToClean = $false

function Write-Stage {
    param([string] $Name)

    $script:stage = $Name
    Write-Output "==> $Name"
}

function Update-ProcessPath {
    $currentPath = $env:Path
    $machinePath = [Environment]::GetEnvironmentVariable('Path', 'Machine')
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    $entries = @($machinePath, $userPath, $currentPath) | Where-Object {
        -not [string]::IsNullOrWhiteSpace($_)
    }
    $env:Path = $entries -join [IO.Path]::PathSeparator
}

function Find-Application {
    param([string] $Name)

    $command = Get-Command $Name -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if ($null -eq $command) {
        return $null
    }
    return $command.Source
}

function Invoke-Checked {
    param(
        [string] $Program,
        [string[]] $Arguments,
        [int[]] $SuccessCodes = @(0)
    )

    # Windows PowerShell 5.1 wraps native stderr as ErrorRecord objects, and
    # `Stop' can turn one localized diagnostic into a premature script error.
    # Normalize both native streams to ordinary UTF-8 log lines, then decide
    # success exclusively from the native exit status.
    $previousErrorPreference = $ErrorActionPreference
    try {
        $ErrorActionPreference = 'Continue'
        & $Program @Arguments 2>&1 | ForEach-Object {
            if ($_ -is [System.Management.Automation.ErrorRecord]) {
                Write-Output ($_.Exception.Message)
            }
            else {
                Write-Output ($_.ToString())
            }
        }
        $status = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorPreference
    }
    if ($status -notin $SuccessCodes) {
        throw "Command exited with status ${status}: $Program $($Arguments -join ' ')"
    }
}

function Test-MiktexPackageInstalled {
    param(
        [string] $Miktex,
        [string] $Package
    )

    $previousErrorPreference = $ErrorActionPreference
    try {
        $ErrorActionPreference = 'Continue'
        $output = @(
            & $Miktex packages info --template '{isInstalled}' $Package 2>&1
        )
        $status = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorPreference
    }
    $lines = @($output | ForEach-Object {
        if ($_ -is [System.Management.Automation.ErrorRecord]) {
            $_.Exception.Message
        }
        else {
            $_.ToString()
        }
    })
    if ($status -ne 0) {
        throw "Could not inspect MiKTeX package ${Package}: $($lines -join '; ')"
    }
    $installed = $lines | Where-Object {
        $_ -match '^(true|false)$'
    } | Select-Object -Last 1
    if ($null -eq $installed) {
        throw "MiKTeX returned no installation state for package $Package"
    }
    return $installed -eq 'true'
}

function Get-InstallerMetadata {
    param(
        [string] $Content,
        [uri] $BaseUri
    )

    $pattern = '(?s)File name:</div>\s*<div[^>]*>\s*' +
        '(?<name>basic-miktex-[A-Za-z0-9.+-]+-x64\.exe)\s*</div>.*?' +
        'SHA-256:</div>\s*<div[^>]*>\s*(?<hash>[0-9a-fA-F]{64})' +
        '\s*</div>.*?href=[''\"](?<href>[^''\"]*\k<name>)[''\"]'
    $match = [regex]::Match($Content, $pattern)
    if (-not $match.Success) {
        throw 'Could not find the current installer and SHA-256 on the MiKTeX download page'
    }
    return [pscustomobject]@{
        Name = $match.Groups['name'].Value
        Hash = $match.Groups['hash'].Value.ToLowerInvariant()
        Uri = [uri]::new($BaseUri, $match.Groups['href'].Value)
    }
}

function Add-DirectoryToProcessPath {
    param([string] $Directory)

    $entries = $env:Path -split [IO.Path]::PathSeparator
    if ($Directory -notin $entries) {
        $env:Path = $Directory + [IO.Path]::PathSeparator + $env:Path
    }
}

function Remove-SetupWorkDirectory {
    param([string] $Directory)

    # A cancelled installer can retain its executable briefly after returning.
    # Retry only the already-validated per-run directory before giving up.
    $delays = @(100, 200, 400, 800, 1600)
    for ($attempt = 0; $attempt -le $delays.Count; $attempt++) {
        try {
            Remove-Item -LiteralPath $Directory -Recurse -Force
            return
        }
        catch {
            if ($attempt -eq $delays.Count) {
                throw
            }
            Start-Sleep -Milliseconds $delays[$attempt]
        }
    }
}

try {
    $normalizedRoot = [IO.Path]::GetFullPath($WorkRoot).TrimEnd(
        [IO.Path]::DirectorySeparatorChar,
        [IO.Path]::AltDirectorySeparatorChar
    )
    $normalizedWork = [IO.Path]::GetFullPath($WorkDirectory).TrimEnd(
        [IO.Path]::DirectorySeparatorChar,
        [IO.Path]::AltDirectorySeparatorChar
    )
    $requiredPrefix = $normalizedRoot + [IO.Path]::DirectorySeparatorChar
    $leaf = [IO.Path]::GetFileName($normalizedWork)
    if (-not $normalizedWork.StartsWith(
            $requiredPrefix,
            [StringComparison]::OrdinalIgnoreCase
        ) -or -not $leaf.StartsWith('run-', [StringComparison]::Ordinal)) {
        throw "Unsafe setup work directory: $WorkDirectory"
    }
    $safeToClean = $true
    New-Item -ItemType Directory -Force -Path $WorkDirectory | Out-Null
    Update-ProcessPath

    $miktex = Find-Application 'miktex.exe'
    if ([string]::IsNullOrWhiteSpace($miktex)) {
        $otherLatex = Find-Application 'latex.exe'
        if (-not [string]::IsNullOrWhiteSpace($otherLatex)) {
            throw "Found a non-MiKTeX LaTeX command at $otherLatex; refusing to install a second TeX distribution"
        }

        Write-Stage 'Discovering the current MiKTeX installer'
        $downloadResponse = Invoke-WebRequest -UseBasicParsing -Uri $DownloadPage
        $metadata = Get-InstallerMetadata $downloadResponse.Content $DownloadPage
        if ($metadata.Uri.Scheme -ne 'https' -or
            $metadata.Uri.Host -ne $DownloadPage.Host) {
            throw "Unexpected MiKTeX installer URL: $($metadata.Uri)"
        }
        $installer = Join-Path $WorkDirectory $metadata.Name

        Write-Stage "Downloading $($metadata.Name)"
        Invoke-WebRequest -UseBasicParsing -Uri $metadata.Uri -OutFile $installer

        Write-Stage 'Verifying the installer'
        $actualHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $installer).Hash.ToLowerInvariant()
        if ($actualHash -ne $metadata.Hash) {
            throw "Installer SHA-256 mismatch: expected $($metadata.Hash), got $actualHash"
        }
        $signature = Get-AuthenticodeSignature -LiteralPath $installer
        if ($signature.Status -eq [System.Management.Automation.SignatureStatus]::Valid) {
            Write-Output "Installer Authenticode signature is valid: $($signature.SignerCertificate.Subject)"
        }
        elseif ($signature.Status -eq [System.Management.Automation.SignatureStatus]::NotSigned) {
            # MiKTeX publishes the Windows installer and its SHA-256 together,
            # but the current official installer is not Authenticode-signed.
            # The matched digest still protects a download served by a CTAN
            # mirror from differing from the file named by the HTTPS page.
            Write-Output 'Installer is not Authenticode-signed; the official SHA-256 matched.'
        }
        else {
            throw "Installer Authenticode verification failed: $($signature.Status)"
        }

        Write-Stage 'Installing per-user MiKTeX'
        Invoke-Checked $installer @('--private', '--unattended') @(0, 3010)
        Update-ProcessPath
        $miktex = Find-Application 'miktex.exe'
        if ([string]::IsNullOrWhiteSpace($miktex)) {
            throw 'MiKTeX installation completed, but miktex.exe is not available in the refreshed PATH'
        }
    }

    $binDirectory = Split-Path -Parent $miktex
    Add-DirectoryToProcessPath $binDirectory

    Write-Stage 'Updating the MiKTeX package database'
    Invoke-Checked $miktex @('packages', 'update-package-database')

    Write-Stage 'Installing the Shuying TeX package baseline'
    $packages = @(
        'preview',
        'mylatexformat',
        'amsmath',
        'amsfonts',
        'graphics',
        'mathtools',
        'xcolor',
        'ulem'
    )
    foreach ($package in $packages) {
        if (Test-MiktexPackageInstalled $miktex $package) {
            Write-Output "Already installed: $package"
        }
        else {
            Write-Output "Installing package: $package"
            Invoke-Checked $miktex @('packages', 'install', $package)
        }
    }

    Write-Stage 'Verifying required programs'
    foreach ($program in @('latex.exe', 'dvisvgm.exe', 'kpsewhich.exe')) {
        if ([string]::IsNullOrWhiteSpace((Find-Application $program))) {
            throw "Required program is unavailable after setup: $program"
        }
    }

    Write-Stage 'Verifying required TeX files'
    $kpsewhich = Find-Application 'kpsewhich.exe'
    $files = @(
        'preview.sty',
        'mylatexformat.ltx',
        'amsmath.sty',
        'amssymb.sty',
        'mathtools.sty',
        'graphicx.sty',
        'xcolor.sty',
        'ulem.sty'
    )
    foreach ($file in $files) {
        $located = & $kpsewhich $file
        if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($located)) {
            throw "Required TeX file is unavailable after setup: $file"
        }
        Write-Output "    $file -> $located"
    }

    $encodedBin = [Convert]::ToBase64String(
        [Text.Encoding]::UTF8.GetBytes($binDirectory)
    )
    Write-Output "SHUYING_MIKTEX_BIN:$encodedBin"
    Write-Output 'Shuying dependency setup completed successfully.'
}
catch {
    [Console]::Error.WriteLine(
        "Shuying setup failed during '$stage': $($_.Exception.Message)"
    )
    $exitCode = 1
}
finally {
    if ($safeToClean -and (Test-Path -LiteralPath $WorkDirectory)) {
        try {
            Remove-SetupWorkDirectory $WorkDirectory
        }
        catch {
            [Console]::Error.WriteLine(
                "Warning: could not remove setup files in ${WorkDirectory}: $($_.Exception.Message)"
            )
        }
    }
}

exit $exitCode
