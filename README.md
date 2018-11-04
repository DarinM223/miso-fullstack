fullstack HTTP example
======================

One thing that is crucial to a pleasant fullstack development setup is auto-reloading when any part
of the backend or frontend is changed. GHCJS doesn't let you do this by default, however JSaddle allows
you to use normal GHC with websockets to simulate GHCJS. The drawback to this is that you need to start
an extra server. Because of CORS, the browser won't let you call the backend anymore because the frontend
is on a different localhost port. The solution is to proxy the HTTP requests from the JSaddle server to the backend.

## Building

Requirements: Nix, Ghcid

First start reflex-platform on two extra shells (preferably created in tmux) using this command:

```
./reflex-platform/try-reflex
```

Then in one shell run:

```
sh start-backend.sh
```

and in the other run:

```
sh start-frontend.sh
```

To test that the frontend is successfully querying the backend, go to `localhost:3003/api/hello/world`.
You should see the response `"Hello world"`.

To build to GHCJS, run this in a try-reflex shell:

```
sh build-ghcjs.sh
```

The generated jsexe folder should be somewhere in the `dist-ghcjs` folder.
