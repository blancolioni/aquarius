package Aquarius.Library is

   Configuration_Error : exception;

   function Initialize return Boolean;
   --  Calls Aquarius.Options.Load, then bootstraps configuration, plugins
   --  and devices. Returns False (having done nothing else) if
   --  Aquarius.Options.Load itself failed (e.g. --help was shown or the
   --  arguments were invalid) -- callers should not call Shut_Down in that
   --  case, since nothing was started. Returns True, having fully
   --  initialized, otherwise. Raises Configuration_Error if the
   --  Aquarius configuration files cannot be located.

   procedure Shut_Down;

   function Configuration_Path return String;

   function Run return Boolean;
   --  Dispatch on the command-line options (Aquarius.Options), calling
   --  Initialize/Shut_Down around whichever command is selected.
   --  Returns True if a command/file argument was handled; False if
   --  there was nothing to do (no file or command given), leaving the
   --  caller free to decide what to do in that case (e.g. launch a UI).

end Aquarius.Library;
