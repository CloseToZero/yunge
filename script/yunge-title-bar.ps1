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
    private const uint MonitorDefaultToNearest = 2;
    private const long WsCaption = 0x00C00000L;
    private const long WsBorder = 0x00800000L;
    private const long WsDlgFrame = 0x00400000L;
    private const long WsThickFrame = 0x00040000L;
    private const uint SwpNoSize = 0x0001;
    private const uint SwpNoMove = 0x0002;
    private const uint SwpNoZOrder = 0x0004;
    private const uint SwpNoActivate = 0x0010;
    private const uint SwpFrameChanged = 0x0020;

    [StructLayout(LayoutKind.Sequential)]
    private struct Rect
    {
        public int Left;
        public int Top;
        public int Right;
        public int Bottom;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct MonitorInfo
    {
        public uint Size;
        public Rect Monitor;
        public Rect Work;
        public uint Flags;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct WindowInfo
    {
        public uint Size;
        public Rect Window;
        public Rect Client;
        public uint Style;
        public uint ExtendedStyle;
        public uint WindowStatus;
        public uint WindowBorderWidth;
        public uint WindowBorderHeight;
        public ushort WindowType;
        public ushort CreatorVersion;
    }

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

    [DllImport("user32.dll")]
    private static extern IntPtr MonitorFromWindow(
        IntPtr window, uint flags);

    [DllImport("user32.dll", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool GetMonitorInfo(
        IntPtr monitor, ref MonitorInfo info);

    [DllImport("user32.dll", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool GetWindowInfo(
        IntPtr window, ref WindowInfo info);

    public static void Update(long handle)
    {
        var window = new IntPtr(handle);
        var style = GetWindowLongPtr(window, GwlStyle).ToInt64();

        if (style == 0)
            throw new Win32Exception(Marshal.GetLastWin32Error());

        var maximized = IsZoomed(window);

        // WS_CAPTION combines WS_BORDER and WS_DLGFRAME.  Keep the resize
        // border, then explicitly fit a maximized client to the work area.
        if (maximized)
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

        if (maximized)
        {
            var monitor = MonitorFromWindow(
                window, MonitorDefaultToNearest);
            var monitorInfo = new MonitorInfo
            {
                Size = (uint) Marshal.SizeOf<MonitorInfo>()
            };
            var windowInfo = new WindowInfo
            {
                Size = (uint) Marshal.SizeOf<WindowInfo>()
            };

            if (!GetMonitorInfo(monitor, ref monitorInfo)
                || !GetWindowInfo(window, ref windowInfo))
                throw new Win32Exception(Marshal.GetLastWin32Error());

            var left = monitorInfo.Work.Left
                - (int) windowInfo.WindowBorderWidth;
            var top = monitorInfo.Work.Top
                - (int) windowInfo.WindowBorderHeight;
            var right = monitorInfo.Work.Right
                + (int) windowInfo.WindowBorderWidth;
            var bottom = monitorInfo.Work.Bottom
                + (int) windowInfo.WindowBorderHeight;

            if (!SetWindowPos(
                    window, IntPtr.Zero, left, top,
                    right - left, bottom - top,
                    SwpNoZOrder | SwpNoActivate))
                throw new Win32Exception(Marshal.GetLastWin32Error());
        }
    }
}
'@

Add-Type -TypeDefinition $source
[YungeTitleBar]::Update($Handle)
