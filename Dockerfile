FROM erlang:25.3.2.8-alpine

# 指定 bash 为默认 shell
SHELL ["/bin/bash", "-c"]

ARG VERSION

RUN apk add git make curl

WORKDIR /code
WORKDIR /data

COPY config /code/config
COPY include /code/include
COPY priv /code/priv
COPY src /code/src
COPY Makefile /code/
COPY rebar3 /code/
COPY rebar.config /code/

RUN cd /code && \
    chmod u+x ./rebar3 && \
    make release_$VERSION && \
    cp _build/$VERSION/rel/erlang_docker/erlang_docker-1.0.0.tar.gz /data/erlang_docker-$VERSION-1.0.0.tar.gz && \
    cd /data && \
    tar zxf erlang_docker-$VERSION-1.0.0.tar.gz

EXPOSE 4369
EXPOSE 8080
EXPOSE 9090

# 必须以 `foreground` 而不是 `daemon` 启动, 因为得让节点在前台运行, 否则容器执行完命令就停止了
CMD ./bin/erlang_docker foreground
