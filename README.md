my ranch
--------

TODO: ASCII art ranch home here, yo

This here's where I keep home directory stuff, mah dotfiles ya see. Over
yonder's my tractor.

I'll give ya a walk 'round the estate in case you wanna poke around. Mi casa es
su casa compadre.


What's in Here
--------------

This is for the diggers.

I'm not gonna provide you with some fancy scripts to clone all of this, symlink
it wholesale, and hook in local customizations and stuff like that. I don't
think anyone would really want that, nor should you, and I don't want a
maintenance burden for my personal config files, thanks.

Nevertheless, I understand the time-honored pastime of dotfile gazing. I've
devoted plenty of procrastination to it myself. If you see something and have a
question about it, want to tell me that it's stupid or cool or whatever, I
welcome you to use the blame view on Github or Bitbucket to find the commit
where it was added and leave a comment. Or, use the issue tracker if you like.

I will throw out a few things that I think are most worthy of perusing:

  - **Vim**
    See `.vim/doc/my-notes.txt`
  - **tmux**: It's not wildly fancy, but I've got a few cute tricks and I like
    my mapping setup. Never bothered with things like Tmuxinator, I don't have
    such complex window layouts and so many active projects as to want to put a
    YAML and Ruby abstraction layer on top of tmux's quite livable scripting
    interface. I just manage a few session files by hand, load some by default
    and the occasional other with a `source-file` call.

    I use a few Vim plugins for getting more out of tmux, particularly
    [Dispatch]. There are one or two OS X-isms in the config that need guards,
    it's been awhile since I've used it on Linux.
  - bash, no zsh. It's the default on basically every system I ever touch.
  - inputrc is cool

This is a [Mercurial] repository, mirrored to Github with the excellent [hg-git]
interoperability extension. This is relevant if you find yourself wondering
where one of the few subrepositories (submodules) come from: hg's subrepo
tracking is not passed through hg-git. You can find the subrepo URLs
[here][hgsub] and their current revisions [here][hgsubstate]. Mainly it's just
NeoBundle for Vim, submodules are a PITA and best kept to a minimum.


#### TODO ####

  - Migrate ctags workflows to [GNU global]
  - Been meaning to switch from irssi to weechat. Plaintext passwords in the
    config file, I mean wtf.

The rest of this document is written more for myself than anyone else. If you're
bored you might get a sense of personal flavor and nostalgia from it.


### History and Histrionics ###

Surely more than any source control repository that we commit to in our careers,
our dotfiles must tell a personal history. There might be one or two of you out
there who take an interest not just in what's in here, but where it came from
and where my trail of interests has taken me over time.

- The source of this is a Mercurial repository; keeps me fresh on Mercurial.

Mercurial and the early days of DVCS in the Python community. Primordial GitHub.
I hold a sentimental soft spot for Hg and its usability.

Shake and Nuke. These tools are remarkably scriptable, programmable, extensible.
The visual effects industry really has a pretty interesting history of software
tooling. These are fun applications for someone with a hacker mentality to use.


[Dispatch]: https://github.com/tpope/vim-dispatch/
[Mercurial]: http://mercurial.selenic.com/
[hg-git]: http://hg-git.github.io/
[hgsub]: https://bitbucket.org/ches/dotfiles/src/master/.hgsub
[hgsubstate]: https://bitbucket.org/ches/dotfiles/src/master/.hgsubstate
[GNU Global]: http://www.gnu.org/software/global/

<!-- vim:set et sw=4 ts=4 tw=80: -->
