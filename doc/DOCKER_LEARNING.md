# Docker Learning <!-- omit in toc -->

- [前言](#前言)
- [版本](#版本)
- [使用](#使用)
  - [构建镜像](#构建镜像)
  - [运行镜像](#运行镜像)
  - [修改系统版本](#修改系统版本)
  - [修改 Erlang 和 Rebar3 版本](#修改-erlang-和-rebar3-版本)
- [Dockerfile 语法](#dockerfile-语法)
  - [FROM](#from)
  - [ENV](#env)
  - [ADD](#add)
  - [COPY](#copy)
  - [WORKDIR](#workdir)
  - [VOLUME](#volume)
  - [USER](#user)
  - [ARG](#arg)
  - [HEALTHCHECK](#healthcheck)
  - [RUN](#run)
  - [EXPOSE](#expose)
  - [CMD](#cmd)
  - [ENTRYPOINT](#entrypoint)

## 前言

使用 `Docker` 将 `Erlang` 服务运行的环境打包成镜像, 便于在任意服务器便捷部署

## 版本

| 类型 | 版本 |
| :--- | :---: |
| 系统 | alpine:3.18 |
| Erlang | 25.3.2.8 |
| Rebar3 | 3.22.1 |

## 使用

### 构建镜像

```bash
make build
```

### 运行镜像

```bash
make run
```

### 修改系统版本

修改 `Dockerfile` 的 `FROM` 标签

```docker
# Dockerfile
FROM alpine:3.18
```

### 修改 Erlang 和 Rebar3 版本

修改 `Dockerfile` 的 `ENV` 标签

```docker
# Dockerfile
ENV OTP_VERSION="25.3.2.8" \
	REBAR3_VERSION="3.22.1"
```

## Dockerfile 语法

### FROM

该指令用于指定基础镜像, 每个 `Dockerfile` 都必须以 `FROM` 开头, 这是镜像构建的基础

```docker
FROM ubuntu:20.04
```

### ENV

该指令用于设置环境变量, 这些环境变量在构建镜像和容器运行时都可以使用

```docker
ENV APP_HOME /app
ENV PORT 8080
```

### ADD

该指令用于将文件和目录复制到镜像中, 并且可以从 `URL` 下载文件和提取压缩文件

```docker
# ADD [当前环境文件路径 或 URL] [镜像系统的路径]
ADD myfile.tar.gz /app
```

### COPY

该指令类似于 `ADD`, 但只支持简单的文件和目录复制, 不支持解压和 URL 下载

```docker
# COPY [当前环境文件路径] [镜像系统的路径]
COPY src/ /app/
```

### WORKDIR

该指令用于设置工作目录, 如果目录不存在, 会自动创建

```docker
WORKDIR /app
```

### VOLUME

该指令创建一个挂载点, 将数据从容器共享到主机或其他容器

```docker
VOLUME /data
```

### USER

该指令用于指定运行容器时的用户

```docker
USER appuser
```

### ARG

该指令定义在构建时传递给构建过程的变量

构建时:

```bash
docker build --build-arg VERSION="1.0.0" -t my_image .
```

`Dockerfile` 会接受构建命令传入的变量:

```docker
ARG VERSION
RUN echo "Version: $VERSION"
```

### HEALTHCHECK

该指令用于指定检查容器健康状况的命令

```docker
HEALTHCHECK --interval=30s --timeout=10s \
  CMD curl -f http://localhost/ || exit 1
```

### RUN

该指令用于在镜像构建过程中执行命令, 并将结果提交为一个新层, 通常用于安装软件包和进行配置

```docker
# 建议同一条 `RUN` 指令的多个命令用 ` && \换行` 拼接
RUN apt-get update && \
    apt-get install -y python3 python3-pip

RUN mkdir -p $APP_HOME
```

### EXPOSE

该指令声明镜像希望监听的端口, 但不会自动发布端口, 用于文档化用途, 帮助用户理解服务在哪些端口上运行

```docker
EXPOSE 80
```

### CMD

该指令用于指定容器启动时执行的命令, 每个 `Dockerfile` 只能有一个 `CMD`, 如果指定了多个, 只有最后一个生效, `CMD` 可以被 `docker run` 命令行参数覆盖

```docker
CMD ["python3", "-m", "http.server", "$PORT"]
# 等同于
CMD python3 -m http.server $PORT
```

### ENTRYPOINT

该指令用于配置容器启动时运行的主命令, 不会被 `docker run` 命令行参数覆盖, 但可以追加参数

```docker
ENTRYPOINT ["python3", "app.py"]
```
