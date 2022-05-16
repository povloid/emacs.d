#!/bin/bash

tmux new -s 'PROD' -d
tmux new -s 'main' -d
tmux new -s 'qa backend' -d
tmux new -s 'qa admin' -d
tmux new -s 'qa web' -d


tmux a
