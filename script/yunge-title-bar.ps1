# SPDX-FileCopyrightText: 2026 Chen Zhexuan
# SPDX-License-Identifier: MIT

param(
    [Parameter(Mandatory = $true)]
    [long] $Handle
)

$source = @'
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

public static class YungeTitleBar
{
    private const int GwlStyle = -16;
    private const long WsCaption = 0x00C00000L;
    private const long WsBorder = 0x00800000L;
    private const long WsDlgFrame = 0x00400000L;
    private const long WsThickFrame = 0x00040000L;
    private const uint SwpNoSize = 0x0001;
    private const uint SwpNoMove = 0x0002;
    private const uint SwpNoZOrder = 0x0004;
    private const uint SwpNoActivate = 0x0010;
    private const uint SwpFrameChanged = 0x0020;

    [DllImport("user32.dll", EntryPoint = "GetWindowLongPtrW",
        SetLastError = true)]
    private static extern IntPtr GetWindowLongPtr(IntPtr window, int index);

    [DllImport("user32.dll", EntryPoint = "SetWindowLongPtrW",
        SetLastError = true)]
    private static extern IntPtr SetWindowLongPtr(
        IntPtr window, int index, IntPtr value);

    [DllImport("user32.dll", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool SetWindowPos(
        IntPtr window, IntPtr insertAfter, int x, int y, int width,
        int height, uint flags);

    [DllImport("user32.dll")]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool IsZoomed(IntPtr window);

    public static void Update(long handle)
    {
        var window = new IntPtr(handle);
        var style = GetWindowLongPtr(window, GwlStyle).ToInt64();

        if (style == 0)
            throw new Win32Exception(Marshal.GetLastWin32Error());

        // WS_CAPTION combines WS_BORDER and WS_DLGFRAME.  Keep the border
        // styles so a maximized client remains inside the taskbar work area.
        if (IsZoomed(window))
            style = (style & ~WsDlgFrame) | WsBorder | WsThickFrame;
        else
            style |= WsCaption | WsThickFrame;

        if (SetWindowLongPtr(window, GwlStyle, new IntPtr(style))
            == IntPtr.Zero)
            throw new Win32Exception(Marshal.GetLastWin32Error());

        const uint flags = SwpNoSize | SwpNoMove | SwpNoZOrder
            | SwpNoActivate | SwpFrameChanged;

        if (!SetWindowPos(window, IntPtr.Zero, 0, 0, 0, 0, flags))
            throw new Win32Exception(Marshal.GetLastWin32Error());
    }
}
'@

Add-Type -TypeDefinition $source
[YungeTitleBar]::Update($Handle)
