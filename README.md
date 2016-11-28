# calculator #

## API

`GET /calculus` with single mandatory parameter query, base64 encoded of a valid
infix expression. The only supported operators is `+` ATM.

Succesful response is a status 200 with JSON body `{error: false, result: RESULT}`
where `RESULT` is the result of the infix expression.

Invalid infix expression will result in a status 400 with body `{error: true,
message: MESSAGE}` where message is the respective error message.

## Build & Run

```sh
$ cd calculator
$ ./sbt
> jetty:start
> browse
```

If `browse` doesn't launch your browser, manually open
[http://localhost:8080/](http://localhost:8080/) in your browser.
