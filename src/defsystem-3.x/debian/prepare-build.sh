#!/bin/sh

[ -d debian ] || exit 1

 3.4i+cvs.2005.03.03

RELEASE=$(head -n1 README | sed 's/.* \([0-9]*\.[0-9]*i\).*$/\1/')
DATE=$(date +%Y.%m.%d)

[ $(pwd | sed 's/.*\/\([^\/]*\)/\1/') == "defsystem3-cvs-master" ]  || exit 3

set -e

cvs -q -z3  update -APd .

VISUAL=nvi EDITOR=nvi dch --newversion "${RELEASE}+${DATE}" --preserve 
DIR=`pwd`

rm -rf ../cl-defsystem3 || true
find . -type f -depth | cpio --pass-through --make-directories --link --preserve-modification-time ../cl-defsystem3
cd ../cl-defsystem3

find . -name CVS -print0 | xargs -0 rm -rvf
find . -name .cvsignore -print0 | xargs -0 rm -v

