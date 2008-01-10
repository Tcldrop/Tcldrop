Tcldrop v0.6.2 - by FireEgl@EFNet <FireEgl@Tcldrop.US> - January 2008

Description:
   Tcldrop is an IRC bot written in pure Tcl..
   It's being modeled after Eggdrop from a Tcl scripters standpoint..

Features:
   It can be run as a stand-alone script,
   or it can be run from inside an Eggdrop.
   You can run as many Tcldrops in one process as you want.

Requirements:
   At least Tcl v8.4.9 is needed.  (http://Tcl.Tk/)
   For background mode to work you'll need either
   Expect (http://Expect.nist.gov/expect.tar.gz)
   or TclX (http://SF.Net/projects/tclx)
   for their [fork] command.
   Cygwin users will also need run.exe (FixMe: Find the URL to it!)

   For the encpass, encrypt, and decrypt commands to work just like Eggdrop's,
   you'll need a load'able blowfish module, namely this one by leprechau@EFNet:
   http://TclCryptography.SF.Net/
   If anyone out there has a pure-Tcl blowfish package then I'd really like to have it!

Notes:
   This project has yet to be officially announced..
   And it's not currently in a useful state, but if you want to
   look at the code and make suggestions then you're welcome.

Goals:
   To do everything that Eggdrop can do:
      * See the History for what has already been done.
      * Linking to Eggdrop. (I'll need help with this.)
      * Sharing userfiles. (I'll need help with this.)
   And things that Eggdrop doesn't have:
      * Script bind'ings that Eggdrop doesn't support.
         a. Be able to "filt" everything, not just DCC's.
         b. More events to bind to.
      * Tk interface.
      * Fix all of the FixMe's in the code.

History:
   v0.1.0 - Project started in October 2001.
          - Layed out the basic structure and set the most of the goals.
   v0.2.0 - Work resumed March 9, 2003.
          - Support for RAW binds added.
          - idx connection tracking added.
          - Initial support for MSGM, MSG, PUBM, PUB, and CTCP binds added.
          - Added telnet support (users only).
          - Initial support for DCC binds.
          - Basic layout of all the files and namespaces are complete.
          - Support for event binds added.
          - Implemented server queues.
          - Initial Userfile support. (needs flag handling support!)
          - Initial Chanfile support. (still needs *udef stuff.)
          - Updated doc/tcl-commands.txt to reflect the currently implemented commands.
          - timer/utimer and related commands complete. With support for repeating timers.
          - channels module is now 90% complete. Supports udef types: flag, int, and str.
          - Rearranged the module files again, now each is in its own subdirectory.
  4/12/03 - Created a ::tcldrop namespace, and now put all the module namespaces under that.
  4/14/03 - PUB, PUBM, MSG, and MSGM binds are now working perfectly.
  4/21/03 - Support for TIME binds added.  Untested at this point.
 11/01/03 - Initial support for connecting to Eggdrops added.
 11/13/03 - Added the blowfish module. (using leprechau's crypto.mod)
   v0.4.0 - I now consider it approximately 40% Complete.
   v0.5.0 - 50% Complete.
   v0.6.0 - 60% Complete.  (April 2005)

Thanks:
   I'd like to thank the following people, as they've given me code,
   suggestions, and fed me bits of knowledge where I was lacking:
      Papillon on EFNet (op in #Tcl)
      elend on EFNet (op in #Tcl)
      RockShox on FreeNode (#Tcl) and EFNet (op in #Tcl)
      winkey on FDFNet (IRC.FDFNet.Net) (op in #Linux)
      stdarg for writing eggdrop1.7/modules/oldbotnet/PROTOCOL
      phrek on EFNet (op in #Tcl) For starting on the transfer and filesys modules.
      Dossy on EFNet (op in #Tcl)
      iad (slann) on EFNet (op in #Tcl, #EggTcl, #Tcldrop) for creating the php-based website.
      Also, I thank all the guys hanging out in #Tcldrop on EFNet...
      Their interest pushes this project forward. =)
      I'd most especially like to thank all the people involved in creating Eggdrop up to v1.6,
      it's been an excellent model for the Tcldrop project.

Authors/Developers (The ones active for this release anyway):
   FireEgl (Philip Moore)

Copyright:
   Copyright (C) by FireEgl (Philip Moore) <FireEgl@Tcldrop.US>
   Released under the GPL.
   See gpl.txt in the archive, or visit http://www.GNU.Org/licenses/gpl.html

Contact:
   If anybody wants to get in touch with me about this project,
   my most dependable email address is FireEgl@GMail.Com
   I mainly hang out on EFNet (in #Tcl, #EggTcl and #Tcldrop), but you can sometimes
   find me on FreeNode, and UnderNet.
   You may (rarely) find me at these:
   FireEgl on AIM
   628598 on ICQ
   FireEgl2003 on Yahoo!
   FireEgl@HotMail.Com on MSN/Hotmail

Websites:
   Tcldrop's main home is www.Tcldrop.US or http://Tcldrop.SF.Net/ or http://Tcldrop.AltURL.Com/ 
