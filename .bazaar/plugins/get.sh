#!/bin/sh

# Really no sense in tracking the actual plugin source in my dotfiles

pushd $HOME/.bazaar/plugins
    bzr branch lp:bzrtools
    bzr branch lp:bzr-explorer explorer
    bzr branch lp:bzr-pager pager
    bzr branch lp:qbzr
    bzr branch http://people.samba.org/bzr/jelmer/bzr-rewrite/trunk rewrite
    bzr branch lp:bzr-loom loom
popd

