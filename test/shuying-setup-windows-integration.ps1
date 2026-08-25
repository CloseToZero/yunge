# SPDX-FileCopyrightText: 2026 Chen Zhexuan
# SPDX-License-Identifier: MIT

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $SetupScript
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Assert-Equal {
    param(
        [object] $Actual,
        [object] $Expected,
        [string] $Label
    )

    if (-not [object]::Equals($Actual, $Expected)) {
        throw "$Label`: expected '$Expected', got '$Actual'"
    }
}

function Assert-Sequence {
    param(
        [object[]] $Actual,
        [object[]] $Expected,
        [string] $Label
    )

    $actualItems = @($Actual)
    $expectedItems = @($Expected)
    if ($actualItems.Count -ne $expectedItems.Count) {
        throw "$Label`: sequence lengths differ"
    }
    for ($index = 0; $index -lt $actualItems.Count; $index++) {
        Assert-Equal `
            $actualItems[$index] $expectedItems[$index] `
            "$Label item $index"
    }
}

function Assert-Throws {
    param(
        [scriptblock] $Action,
        [string] $Message,
        [string] $Label
    )

    try {
        & $Action | Out-Null
    }
    catch {
        if ($_.Exception.Message -notlike "*$Message*") {
            throw (
                "$Label`: expected error containing '$Message', got '" +
                $_.Exception.Message + "'"
            )
        }
        return
    }
    throw "$Label`: expected an error"
}

$root = Join-Path (
    [IO.Path]::GetTempPath()
) ("shuying-setup-contract-" + [guid]::NewGuid().ToString('N'))
$work = Join-Path $root 'run-contract'
$downloadPage = [uri] 'https://miktex.org/download'
New-Item -ItemType Directory -Force -Path $work | Out-Null

try {
    . $SetupScript `
        -WorkDirectory $work `
        -WorkRoot $root `
        -DownloadPage $downloadPage `
        -ImportFunctions

    Assert-SetupWorkDirectory $work $root
    Assert-Throws {
        Assert-SetupWorkDirectory (
            Join-Path ($root + '-outside') 'run-contract'
        ) $root
    } 'Unsafe setup work directory' 'outside work directory'
    Assert-Throws {
        Assert-SetupWorkDirectory (Join-Path $root 'ordinary') $root
    } 'Unsafe setup work directory' 'unowned work directory name'

    $hash = '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'
    $page = @"
File name:</div><div>basic-miktex-24.1-x64.exe</div>
SHA-256:</div><div>$hash</div>
<a href="/download/basic-miktex-24.1-x64.exe">download</a>
"@
    $metadata = Get-InstallerMetadata $page $downloadPage
    Assert-Equal $metadata.Name 'basic-miktex-24.1-x64.exe' 'installer name'
    Assert-Equal $metadata.Hash $hash 'installer hash'
    Assert-Equal $metadata.Uri.Host 'miktex.org' 'installer host'

    Assert-InstallerUri $metadata.Uri $downloadPage
    Assert-Throws {
        Assert-InstallerUri `
            ([uri] 'http://miktex.org/installer.exe') $downloadPage
    } 'Unexpected MiKTeX installer URL' 'insecure installer URI'
    Assert-Throws {
        Assert-InstallerUri `
            ([uri] 'https://example.invalid/installer.exe') $downloadPage
    } 'Unexpected MiKTeX installer URL' 'foreign installer URI'

    Assert-InstallerHash $hash $hash.ToUpperInvariant()
    Assert-Throws {
        Assert-InstallerHash $hash ('f' * 64)
    } 'Installer SHA-256 mismatch' 'installer hash mismatch'

    $validSignature = [pscustomobject]@{
        Status = [System.Management.Automation.SignatureStatus]::Valid
        SignerCertificate = [pscustomobject]@{ Subject = 'CN=Test Signer' }
    }
    $validOutput = @(Confirm-InstallerSignature $validSignature) -join "`n"
    if ($validOutput -notlike '*CN=Test Signer*') {
        throw 'valid Authenticode signer was not reported'
    }

    $unsignedSignature = [pscustomobject]@{
        Status = [System.Management.Automation.SignatureStatus]::NotSigned
        SignerCertificate = $null
    }
    $unsignedOutput = @(
        Confirm-InstallerSignature $unsignedSignature
    ) -join "`n"
    if ($unsignedOutput -notlike '*SHA-256 matched*') {
        throw 'unsigned installer did not require the matched digest'
    }

    $invalidSignature = [pscustomobject]@{
        Status = [System.Management.Automation.SignatureStatus]::HashMismatch
        SignerCertificate = $null
    }
    Assert-Throws {
        Confirm-InstallerSignature $invalidSignature
    } 'Authenticode verification failed' 'invalid Authenticode signature'

    $global:ShuyingTestArguments = @()
    $global:ShuyingTestExitCode = 7
    function global:Shuying-TestNative {
        $global:ShuyingTestArguments = @($args)
        $global:LASTEXITCODE = $global:ShuyingTestExitCode
        Write-Output 'native output'
    }
    $nativeOutput = @(
        Invoke-Checked `
            'Shuying-TestNative' @('alpha', 'two words') @(7)
    ) -join "`n"
    Assert-Sequence `
        $global:ShuyingTestArguments @('alpha', 'two words') `
        'native arguments'
    if ($nativeOutput -notlike '*native output*') {
        throw 'native command output was not forwarded'
    }
    $global:ShuyingTestExitCode = 9
    Assert-Throws {
        Invoke-Checked 'Shuying-TestNative' @('failed') @(0)
    } 'Command exited with status 9' 'native command failure'

    $global:ShuyingTestMiktexArguments = @()
    $global:ShuyingTestMiktexExitCode = 0
    $global:ShuyingTestMiktexOutput = 'true'
    function global:Shuying-TestMiktex {
        $global:ShuyingTestMiktexArguments = @($args)
        $global:LASTEXITCODE = $global:ShuyingTestMiktexExitCode
        Write-Output $global:ShuyingTestMiktexOutput
    }
    Assert-Equal (
        Test-MiktexPackageInstalled 'Shuying-TestMiktex' 'preview'
    ) $true 'installed package state'
    Assert-Sequence `
        $global:ShuyingTestMiktexArguments `
        @('packages', 'info', '--template', '{isInstalled}', 'preview') `
        'MiKTeX package query'

    $global:ShuyingTestMiktexOutput = 'false'
    Assert-Equal (
        Test-MiktexPackageInstalled 'Shuying-TestMiktex' 'xcolor'
    ) $false 'missing package state'
    $global:ShuyingTestMiktexOutput = 'unknown'
    Assert-Throws {
        Test-MiktexPackageInstalled 'Shuying-TestMiktex' 'xcolor'
    } 'no installation state' 'malformed package state'
    $global:ShuyingTestMiktexExitCode = 5
    $global:ShuyingTestMiktexOutput = 'query failed'
    Assert-Throws {
        Test-MiktexPackageInstalled 'Shuying-TestMiktex' 'xcolor'
    } 'Could not inspect MiKTeX package' 'package query failure'

    $global:ShuyingTestArguments = @()
    $global:ShuyingTestExitCode = 0
    Set-MiktexAutomaticPackageInstallation 'Shuying-TestNative'
    Assert-Sequence `
        $global:ShuyingTestArguments `
        @('--set-config-value=[MPM]AutoInstall=1') `
        'automatic package installation configuration'

    Assert-Sequence `
        @(Get-ShuyingMiktexPackages) `
        @(
            'preview',
            'mylatexformat',
            'amsmath',
            'amsfonts',
            'graphics',
            'mathtools',
            'xcolor',
            'ulem',
            'cm-super',
            'xetex',
            'fontspec',
            'xecjk',
            'ctex',
            'zhnumber'
        ) `
        'MiKTeX package baseline'
    Assert-Sequence `
        @(Get-ShuyingRequiredTexFiles) `
        @(
            'preview.sty',
            'mylatexformat.ltx',
            'amsmath.sty',
            'amssymb.sty',
            'mathtools.sty',
            'graphicx.sty',
            'xcolor.sty',
            'ulem.sty',
            'cm-super-t1.enc',
            'sfrm1000.pfb',
            'fontspec.sty',
            'xeCJK.sty',
            'ctex.sty',
            'zhnumber.sty'
        ) `
        'required TeX files'

    # Windows PowerShell 5.1 can decode a BOM-less script as an older code
    # page.  Build the marker from code points while still testing UTF-8 output.
    $unicodeMarker = [string]::Concat(
        [char]0x4e2d,
        [char]0x6587
    )
    Write-Output "Windows setup behavior tests passed: $unicodeMarker"
}
finally {
    Remove-Item Function:\Shuying-TestNative -ErrorAction SilentlyContinue
    Remove-Item Function:\Shuying-TestMiktex -ErrorAction SilentlyContinue
    Remove-Variable ShuyingTestArguments -Scope Global `
        -ErrorAction SilentlyContinue
    Remove-Variable ShuyingTestExitCode -Scope Global `
        -ErrorAction SilentlyContinue
    Remove-Variable ShuyingTestMiktexArguments -Scope Global `
        -ErrorAction SilentlyContinue
    Remove-Variable ShuyingTestMiktexExitCode -Scope Global `
        -ErrorAction SilentlyContinue
    Remove-Variable ShuyingTestMiktexOutput -Scope Global `
        -ErrorAction SilentlyContinue
    if (Test-Path -LiteralPath $root) {
        Remove-Item -LiteralPath $root -Recurse -Force
    }
}
