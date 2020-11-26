#!/bin/sh
xpanes -c "{}" "cd clojure && PORT=8081 lein ring server-headless" "cd prolog && ./server.pl --port=8082 --no-fork" "npm run dev"
