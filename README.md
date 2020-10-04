# backend for MVIL app

Built with stack + nix

Health server at :8080/health

heroku login
docker ps
heroku container:login
heroku container:push web
heroku container:release web

## Run with mock server

    cd integrationtest
    docker build . 
    docker run <img> -P

find the port number xxx

    DATABASE_URL=postgres://posts:apekatt@localhost:xxx/posts stack repl