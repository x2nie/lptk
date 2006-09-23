// lp2config.inc

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

{$ifndef win32}
{$define x11}
{$endif}

// use double buffering for the drawings?
{$DEFINE BUFFERING}

