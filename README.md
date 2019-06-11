# Pomohoro

I like the pomodoro technique and I use the timeclock format to keep track of
how much time I spend on tasks (through hledger.) I wanted something that
combined both of these, so I created Pomohoro. I recommend something along
the lines of

    alias ph=pomohoro

which is the alias I will use throughout this document.


## Installation

To install, the following four commands are likely sufficient:

1. Install the Haskell build tool Stack. This is not strictly necessary, but
    it will make installation much easier than having to chase dependencies
    and compile manually.

        $ sudo apt-get install stack

2. Download the code.

        $ git clone git@github.com:kqr/Pomohoro.git && cd Pomohoro

3. Build and install the program.

        $ stack install

4. Put the executable in your path (adjust command to suit your path preferences.)

        $ sudo cp ~/.local/bin/pomohoro-exe /usr/local/bin/pomohoro

The third command may exit with an error and prompt you to run `stack setup`. If
it does, follow those instructions. It is simply saying that it can't find the
correct version of the compiler on your system, so it will download one an
install it in a sandbox so it does not affect the rest of your system.


## Basic usage

With the above alias, all you need to start the timer is

    ph start

This should output nothing, but 25 minutes after you issued the command, the
timer will go off and it will display a FreeDesktop.org notification alerting
you of this. 25 minutes of work will be recorded to the timelog file, which by
default is called `.pomohoro.timeclock` and is located in your home directory.

You can use hledger to get a summary of this time information rendered in
several neat formats. One of my favourites is the daily balance, which you get
by running `hledger -f timeclock_file balance --daily`:

    Balance changes in 2016/10/02-2016/10/08:

             ||  2016/10/02  2016/10/03  2016/10/04  2016/10/05  2016/10/06  2016/10/07  2016/10/08
    =========++=====================================================================================
     dd2380  ||       3.90h           0       2.26h       2.51h       1.27h       3.17h           0
     ik2218  ||           0           0       1.25h       0.18h       2.00h           0       2.00h
     org     ||       0.25h       5.58h       1.70h       0.32h           0       0.42h           0
     support ||           0           0           0       0.19h           0           0           0
    ---------++-------------------------------------------------------------------------------------
             ||       4.15h       5.58h       5.21h       3.20h       3.27h       3.58h       2.00h

If you after your hard working session want to take a timed five-minute break,
you can start that with

    ph rem 5

As you can guess, the number `5` represents how many minutes until you want
to be reminded that it's time to get back to work.

If your pomodoro session gets interrupted, you signal this by issuing the
command

    ph int

which will terminate your current session and record the correct starting and
stopping times in the timeclock file. Note: this will only terminate an active
session, it will not cancel any reminders you have set!


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

You can also check how far into your current session you are, by issuing the
command

    ph status

which will respond with something along the lines of

    Current work session: 19/25 minute(s)

If you want a shorter session than usual, but you suck at keeping track of time
yourself, you can always start a regular session along with a reminder, and
then manually interrupt the regular session once the timer goes off. Like so:

    ph start
    ph rem 15   # I only really want a 15 minute session
    # ...
    # reminder goes off!
    ph int

As it happens, you can use the reminder functionality as a general reminding
tool, for example like this:

    ph rem 20 Drink some more water!

where it will include your message in the reminder.


## Further reading

I will collect some links here. About hledger, timeclock and the pomodoro technique.

* [Blog post about Pomohoro](https://two-wrongs.com/pomohoro-combine-pomodoro-technique-hledger-timeclock-productivity)


## Developer notes

### Building

This project is stackified, so the easiest way to build it is to run

    stack build

and then follow the instructions on the screen.


### Todo

Roughly in order of priority:

* Convenience wrapping command around hledger (or the hledger API?) to get
    the time balance for the last week or something?

* Tests

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

    It does however require an upgrade to the protocol. You're likely to also
    want to get the status of either all sessions or a specific one.

