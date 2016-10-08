# Pomohoro

I like the pomodoro technique and I use the timeclock format to keep track of
how much time I spend on tasks (through hledger.) I wanted something that
combined both of these, so I created Pomohoro. I recommend something along
the lines of

    alias ph=pomohoro


## Basic usage

With the above alias, all you need to start the timer is

    ph start

This should output nothing, but 25 minutes after you issued the command, the
timer will go off and it will display a FreeDesktop.org notification alerting
you of this. 25 minutes of work will be recorded to the timelog file, which by
default is called `.pomohoro.timeclock` and is located in your home directory.

If you then want to take a timed five-minute break, you can start that with

    ph rem 5

As you can guess, the number `5` represents how many minutes until you want
to be reminded that it's time to get back to work. As it happens, you can use
this as a general reminding tool, for example like this:

    ph rem 20 Drink some more water!

where it will include your message in the reminder.

If your pomodoro session gets interrupted, you signal this by issuing the
command

    ph int

which will terminate your current session and record the correct starting and
stopping times in the timeclock file.


## Configuration

The configuration file should be located in your home directory and be named
`.pomohoro.cfg`. Here's a sample you can tweak to your liking:

    # How long is the work session started with Pomohoro start?
    session-length = 25

    # Which timeclock file do you want Pomohoro to append times to?
    timeclock-file = "/home/johns/.hledger/side-projects.timeclock"

    # This is used in the timeclock file if no account is specified on
    # the command line
    default-account = "work"

    # Which UDP port do you want Pomohoro to listen to and send messages to?
    port = 8712

If you don't understand what a setting means, you can most likely skip it
entirely and still be happy.


## Advanced usage

If you want to start working on a specific client or task and have it tracked
separately in the timeclock file, you can name the account on the command line,
like so:

    ph start acme

If you want to be even more specific than that, you can also include a comment
saying what you're doing for Acme.

    ph start acme annoying newline bug in web shop

If you want a shorter session than usual, but you suck at keeping track of time
yourself, you can always start a regular session along with a reminder, and
then manually interrupt the regular session once the timer goes off. Like so:

    ph start
    ph rem 15   # I only really want a 15 minute session
    # ...
    # reminder goes off!
    ph int 


## Developer notes

### Building

This project is stackified, so the easiest way to build it is to run

    stack build

and then follow the instructions on the screen.


### Todo

* Multiple simultaneous sessions. Useful if you want to double-bill clients,
    e.g. you're working on a feature that both need and should pay for. Easiest
    to explain with the following sequence of commands:

        ph start acme      # acme needs this feature
                           # some time passes
        ph start oscorp    # hey, oscorp can use this part of the feature too
                           # additional time passes
        ph int oscorp      # okay, this is stuff oscorp doesn't want
                           # more time
        ph int             # end all remaining sessions

    This should be pretty easy to make since UDP allows several processes to
    listen to a single port.

* Tests
