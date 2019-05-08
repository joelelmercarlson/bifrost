CWD     := $(shell pwd)
ORGPATH := $(CWD)
STACK   := stack
BUILD   := $(STACK) build
COMMIT  := $(ORGPATH)/commit.hs
RUN     := $(STACK) exec Main
RPMS    := freeglut-devel libXi-devel libXrandr-devel libXi-devel libXcursor-devel libXinerama-devel zlib-devel
YUM     := sudo dnf 

GIT       := git
GITDIFF   := $(GIT) diff
GITLOG    := $(GIT) log
GITSTATUS := $(GIT) status
GITPULL   := $(GIT) pull
GITPUSH   := $(GIT) push

build:
	$(BUILD)

commit:
	$(COMMIT)

diff:
	$(GITDIFF)

log:
	$(GITLOG)

monkey:
	$(RUN) json/monkey.json

pull:
	$(GITPULL)

push:
	$(GITPUSH)

rhel:
	$(YUM) install -y $(RPMS)

run:
	$(RUN)

status:
	$(GITSTATUS)

upgrade:
	$(STACK) upgrade

version:
	$(STACK) --version
