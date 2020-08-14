# Neil
![Neil](https://raw.githubusercontent.com/NeilProject/Neil/master/Stuff/Mascotte.png)


Neil is a programming language translating real-time to Lua. It requires Lua 5.1 to run (although you can consider 5.1 support to be deprecated from the start), but 5.2 or later is recommended (some features even require 5.2, simply because 5.1 didn't support the things I needed to make some effects happen).

Neil has been made to make projects requiring more code than a simple add-on would a bit easier. Lua was made for simple-addons, but as Lua is more and more being used for large big projects, Lua's freedom can cause you a lot of issues with debugging. This is where Neil comes in. Neil is by far more restrictive than Lua, but these restrictions will make some bugs that would normally lead you to big mysteries, easier to deal with.

- Neil requires all identifiers to be declared prior to usage. Usage of undeclared variables will cause errors. (This works wonders to prevent typos to be never found).
- Neil does have a kind of semi-strong-typing, so protect you against type messups. Trying to define a string to a number variable will make Neil go "KABOOM!"
- Neil is case INSENSITIVE, for both its keywords AND its identifiers, in spite of Lua being case sensitive. So MyVariable and myvariable and MYVARIABLE are all the same for Neil. Sorry, I learned how to code with Basic and Pascal, and then case sensitive languages are always a nightmare. 
- Neil has its own variable system, but can still easily access all Lua global identifiers easily. "Lua.print("Hello World!")" will just call Lua's print directly.
- Neil can interface with Lua though, for example by means of the "Lua" table, as shown above, but also with "#pure" and "#endpure" which do allow you to code pure Lua code without Neil interfering in case you need this.
- Neil offers a few things that Lua does not, although Lua can do it by means of dirty looking code. Neil can do it the neat way. Switch statements, and Try-Catch blocks for example. Neil also offers "defer" support (comparable to the keyword "defer" in the Go programming language). Neil also has a clean syntax for optional function parameters.

# Hello World

Let's not break with traditions. So here goes the simplest version of Hello World in Neil

~~~
Init
       Print("Hello World")
End
~~~

A bit more complex of an example, but hey, now we're into this:
~~~
Void Hello()
       Print("Hello World")
End

Init
    Hello()
End
~~~


# Name of the language

Neil was named after the first man on the moon, merely as a pun to the name of the underlying scripting engine Lua, which means "moon" in Portuguese. Yes, the creators of Lua knew that, since Portuguese is their first language, and the Lua was named after the moon. 

And don't send any complaints my way that I believe in the moonlanding, and that it was all a hoax because a) I am NOT interested in already debunked conspiracy theories, and b) even if it was a hoax, the pun still works. So I can only answer such complaints by instant bans, as I don't have time for those kinds of discussions. It's sad that I need to put that disclaimer in, but hey, some people are persistent... this is the internet, after all. 😜

# Is your language a small step for coding, but a giant leap for the coding kind then?

I dunno. Not up to me to decide. For me Neil is merely filling a void that made me suffer, nothing more. When people were asking me about languages translating to C and if it was better to use C or that transcompiler, I always say "When you want the full power of C, use C. If you want something easier than C, use that transcompiler". What is most important to you as a coder is always the key. Technically this goes for ALL languages which are merely transcompiling to other languages, no exceptions.  And neither is Neil, for that matter. You will find out that Neil will translate loads of stuff almost literally to Lua where it can, but due to it being dependent on a few more error checkers (which you can turn off) it might be a tiny bit slower on a few departments. Of course, compiling the script WILL take a bit longer, since Neil first has to translate it to pure Lua code, and then Lua does the actual compiling. In run-time, I expect most of your code to be as fast as if it were written in pure Lua from the start. 

# Jeroen, we have a problem!

If you encounter trouble, first always look at the [Troubleshooting](https://github.com/NeilProject/Neil/wiki/Troubleshooting) document on the wiki here, to see if your problem has been taken care of, or has a reason. Lua does not provide some features Neil can need in some situations, simply because Lua has no support for them, and Lua has no support for them because Lua had to be compatible with all C compilers and C does not provide a standard library for that (When trying to use Neil Bundles you will experience this most. In any other kinds of usage you are probably fine). In case you really think you found a bug in Neil, go to my [issue tracker](https://github.com/NeilProject/Neil/issues) and see if I either already made note of the bug myself, or if other people did report it to me already. 

Important note! Be polite when reporting trouble. I am a human with feelings. Bug reports with stuff like "Fix! Or I'll be uninstalling!" or "Fix! FFS!" or any other kinds of rude behavior will lead to your report being closed, ignored, and in extreme cases the added bonus of being banned from this repository. It goes without saying, but unfortunately some people need this warning anyway!

Also, if possible, please provide some sample Neil code that causes the trouble to happen. Saves me tons of time to sort things out.

