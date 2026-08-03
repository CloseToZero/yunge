# SPDX-FileCopyrightText: 2026 Chen Zhexuan
# SPDX-License-Identifier: MIT

param(
    [Parameter(Mandatory, Position = 0, ValueFromRemainingArguments)]
    [string[]] $LiteralPath
)

Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;

public static class YungeFileManager
{
    private const int RpcEChangedMode = unchecked((int)0x80010106);

    [DllImport("ole32.dll")]
    private static extern int CoInitializeEx(IntPtr reserved, uint model);

    [DllImport("ole32.dll")]
    private static extern void CoTaskMemFree(IntPtr memory);

    [DllImport("ole32.dll")]
    private static extern void CoUninitialize();

    [DllImport("shell32.dll")]
    private static extern IntPtr ILFindLastID(IntPtr itemIdList);

    [DllImport("shell32.dll", CharSet = CharSet.Unicode,
        PreserveSig = true)]
    private static extern int SHParseDisplayName(
        string name,
        IntPtr bindingContext,
        out IntPtr itemIdList,
        uint attributesIn,
        out uint attributesOut);

    [DllImport("shell32.dll", PreserveSig = true)]
    private static extern int SHOpenFolderAndSelectItems(
        IntPtr folder,
        uint itemCount,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]
        IntPtr[] items,
        uint flags);

    private static void ThrowIfFailed(int result)
    {
        if (result < 0)
            Marshal.ThrowExceptionForHR(result);
    }

    public static void Reveal(string directory, string[] paths)
    {
        int initializeResult = CoInitializeEx(IntPtr.Zero, 2);
        bool uninitialize = initializeResult >= 0;
        if (initializeResult < 0 && initializeResult != RpcEChangedMode)
            ThrowIfFailed(initializeResult);

        IntPtr folder = IntPtr.Zero;
        IntPtr[] absoluteItems = new IntPtr[paths.Length];
        try
        {
            uint attributes;
            ThrowIfFailed(SHParseDisplayName(
                directory, IntPtr.Zero, out folder, 0, out attributes));

            IntPtr[] childItems = new IntPtr[paths.Length];
            for (int index = 0; index < paths.Length; ++index)
            {
                ThrowIfFailed(SHParseDisplayName(
                    paths[index], IntPtr.Zero,
                    out absoluteItems[index], 0, out attributes));
                childItems[index] = ILFindLastID(absoluteItems[index]);
            }

            ThrowIfFailed(SHOpenFolderAndSelectItems(
                folder, (uint)childItems.Length, childItems, 0));
        }
        finally
        {
            foreach (IntPtr item in absoluteItems)
                if (item != IntPtr.Zero)
                    CoTaskMemFree(item);
            if (folder != IntPtr.Zero)
                CoTaskMemFree(folder);
            if (uninitialize)
                CoUninitialize();
        }
    }

}
'@

$paths = foreach ($path in $LiteralPath) {
    [IO.Path]::GetFullPath($path)
}

$shell = New-Object -ComObject Shell.Application

$paths |
    Group-Object { [IO.Path]::GetDirectoryName($_) } |
    ForEach-Object {
        $directory = $_.Name
        [YungeFileManager]::Reveal($directory, [string[]] $_.Group)

        # PowerShell starts hidden, so reveal only the Explorer window.
        $location = ([Uri] $directory).AbsoluteUri.TrimEnd('/')
        $window = $null
        for ($attempt = 0; $attempt -lt 20; ++$attempt) {
            $window = $shell.Windows() |
                Where-Object {
                    $_.LocationURL -and
                    $_.LocationURL.TrimEnd('/') -ieq $location
                } |
                Select-Object -Last 1
            if ($window) {
                break
            }
            Start-Sleep -Milliseconds 50
        }
        if (-not $window) {
            throw "Explorer did not open $directory"
        }
        $window.Visible = $true
    }
