# Erlang K8S Docker <!-- omit in toc -->

- [前言](#前言)
- [Multipass](#multipass)
- [Docker](#docker)
- [kubeadm 和 kubelet](#kubeadm-和-kubelet)
- [K3S](#k3s)
- [Docker Desktop](#docker-desktop)
- [Erlang 代码](#erlang-代码)
- [镜像](#镜像)
  - [Dockerfile](#dockerfile)
  - [打包镜像](#打包镜像)
  - [加载镜像](#加载镜像)
  - [本地镜像仓库](#本地镜像仓库)
  - [上传镜像](#上传镜像)
- [配置目录](#配置目录)
- [StorageClass 配置](#storageclass-配置)
- [a版节点配置](#a版节点配置)
  - [Service 配置](#service-配置)
  - [Headless Service 配置](#headless-service-配置)
  - [StatefulSet 配置](#statefulset-配置)
- [b版节点配置](#b版节点配置)
  - [Service 配置](#service-配置-1)
  - [Headless Service 配置](#headless-service-配置-1)
  - [StatefulSet 配置](#statefulset-配置-1)
- [应用配置](#应用配置)
- [测试](#测试)
  - [打包并发布](#打包并发布)
  - [http](#http)
  - [websocket](#websocket)
- [异常与解决方法](#异常与解决方法)
  - [镜像启动](#镜像启动)
  - [Pod 的 DNS](#pod-的-dns)
  - [节点唯一名字](#节点唯一名字)
  - [集群内部 `service` 域名地址](#集群内部-service-域名地址)
  - [反复请求 `service` 接口负载均衡](#反复请求-service-接口负载均衡)

## 前言

- 本地开发环境, 使用 `Multipass` 安装 `Ubuntu` 虚拟机, 其中 1 个作为 `Master` 节点, 另外 3 个作为 `Woker` 节点
- 使用 `Docker` 作为容器
- 使用 `K3S` 搭建多节点集群环境
- [`Docker` 相关使用](./doc/DOCKER_LEARNING.md)
- [`K8S` 相关使用](./doc/K8S_LEARNING.md)
- `erlang` 节点共用一套代码, 打包成a版和b版, a版 3 个 `pod`, b版 3 个 `pod`

## Multipass

安装 `Multipass`, 进入官网根据当前系统选择安装包

> [Multipass 官网下载安装包](https://multipass.run/install)

创建 `Master` 虚拟机

```bash
multipass launch --name master --cpus 2 --memory 8G --disk 10G
```

创建 `Worker` 虚拟机

```bash
multipass launch --name worker1  --cpus 2 --memory 8G --disk 10G
multipass launch --name worker2  --cpus 2 --memory 8G --disk 10G
multipass launch --name worker3  --cpus 2 --memory 8G --disk 10G
```

创建公钥

```bash
ssh-keygen -t rsa -b 4096
cat .ssh/id_rsa.pub
```

分别进入 4 个虚拟机

```bash
multipass shell master                      # 每个虚拟机都做下面这些操作
sudo passwd ubuntu                          # 修改密码
sudo apt update && sudo apt upgrade -y      # 更新系统
vi ~/.ssh/authorized_keys                   # 将上面的公钥添加进该文件
```

退出虚拟机回到本地 `shell`, 修改 `.ssh/config`, 先获取 4 个虚拟机的 `ip`

```bash
multipass list
```

将每个虚拟机的 `ip` 替换下面配置的 `HostName` 参数

```config
Host *
    StrictHostKeyChecking no

Host master
    HostName {MasterIP}
    User ubuntu

Host worker1
    HostName {Worker1IP}
    User ubuntu

Host worker2
    HostName {Worker2IP}
    User ubuntu

Host worker3
    HostName {Worker3IP}
    User ubuntu
```

这样可以直接 `ssh master` 的方式连接虚拟机

## Docker

安装 `Docker`, 分别进入每个虚拟机, 执行:

```bash
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin -y
```

## kubeadm 和 kubelet

安装 `kubeadm` 和 `kubelet`, 分别进入每个虚拟机, 执行:

```bash
CNI_PLUGINS_VERSION="v1.3.0"
ARCH="amd64"
DEST="/opt/cni/bin"
sudo mkdir -p "$DEST"
curl -L "https://github.com/containernetworking/plugins/releases/download/${CNI_PLUGINS_VERSION}/cni-plugins-linux-${ARCH}-${CNI_PLUGINS_VERSION}.tgz" | sudo tar -C "$DEST" -xz
DOWNLOAD_DIR="/usr/local/bin"
sudo mkdir -p "$DOWNLOAD_DIR"
CRICTL_VERSION="v1.30.0"
ARCH="amd64"
curl -L "https://github.com/kubernetes-sigs/cri-tools/releases/download/${CRICTL_VERSION}/crictl-${CRICTL_VERSION}-linux-${ARCH}.tar.gz" | sudo tar -C $DOWNLOAD_DIR -xz
RELEASE="$(curl -sSL https://dl.k8s.io/release/stable.txt)"
ARCH="amd64"
cd $DOWNLOAD_DIR
sudo curl -L --remote-name-all https://dl.k8s.io/release/${RELEASE}/bin/linux/${ARCH}/{kubeadm,kubelet}
sudo chmod +x {kubeadm,kubelet}
RELEASE_VERSION="v0.16.2"
curl -sSL "https://raw.githubusercontent.com/kubernetes/release/${RELEASE_VERSION}/cmd/krel/templates/latest/kubelet/kubelet.service" | sed "s:/usr/bin:${DOWNLOAD_DIR}:g" | sudo tee /usr/lib/systemd/system/kubelet.service
sudo mkdir -p /usr/lib/systemd/system/kubelet.service.d
curl -sSL "https://raw.githubusercontent.com/kubernetes/release/${RELEASE_VERSION}/cmd/krel/templates/latest/kubeadm/10-kubeadm.conf" | sed "s:/usr/bin:${DOWNLOAD_DIR}:g" | sudo tee /usr/lib/systemd/system/kubelet.service.d/10-kubeadm.conf
sudo systemctl enable --now kubelet
```

## K3S

`K3S` 是 `K8S` 的轻量化实现, 先在 `Master` 虚拟机安装 `K3S`

```bash
ssh master
sudo curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC="--docker" sh -
```

查看节点

```bash
sudo kubectl get nodes
```

退出回到本地 `shell`, 为了方便给后面的其他虚拟机创建节点并注册到 `Master` 节点, 在本地获取 `Master` 信息批量创建

获取 `Master` 节点 `TOKEN` 和 `IP`

```bash
TOKEN=$(multipass exec master sudo cat /var/lib/rancher/k3s/server/node-token)
echo $TOKEN
MASTER_IP=$(multipass info master | grep IPv4 | awk '{print $2}')
echo $MASTER_IP
```

循环在 3 个 `worker` 虚拟机安装 `K3S` 并使用 `master` 的 `IP` 和 `TOKEN` 让节点注册到 `master`

```bash
for f in 1 2 3 ; do
    multipass exec worker$f -- bash -c "curl -sfL https://get.k3s.io | K3S_URL=\"https://$MASTER_IP:6443\" K3S_TOKEN=\"$TOKEN\" INSTALL_K3S_EXEC=\"--docker\" sh -"
done
```

进入 `master`, 应该就可以看到有 3 个 `worker` 节点注册到了 `master`

```bash
ssh master
sudo kubectl get nodes
```

结果大概如下:

```bash
NAME      STATUS   ROLES                  AGE   VERSION
master    Ready    control-plane,master   36m   v1.29.5+k3s1
worker1   Ready    <none>                 33m   v1.29.5+k3s1
worker2   Ready    <none>                 33m   v1.29.5+k3s1
worker3   Ready    <none>                 32m   v1.29.5+k3s1
```

## Docker Desktop

本地机器需要安装 `Docker Desktop`, 用于本地镜像打包

[Docker Desktop 下载](https://www.docker.com/products/docker-desktop/), 根据当前系统选择安装即可

安装后需要启动, 否则后续打包命令无法使用

## Erlang 代码

自行查看

- config/
- include/
- src/
- rebar.config
- Makefile

大概逻辑是, 节点 `rpc` 通讯前, 先 `http` 请求对应的 `svc` 服务, 让其负载均衡转发到对应的 `pod`, 并返回对应 `pod` 的 `erlang` 节点名字, 再通过该节点名字进行 `rpc` 通讯

其中重点是 `erlang_docker_http_server.erl`, 每次 `http` 请求 `svc` 都要配置 `{pool, false}` 关闭连接池, 请求成功后 `hackney:close(ClientRef)` 显式关闭连接, 避免复用连接导致无法负载均衡

```erl
%% @doc http 请求
request(Method, Url, Router, Body) ->
    NewUrl = Url ++ Router,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    BodyBin = jsx:encode(Body),
    % {pool, false}: 禁用连接池确保每次请求都不复用连接, 否则复用会导致无法负载均衡
    Options = [{pool, false}, {recv_timeout, 5000}],
    case catch hackney:request(Method, NewUrl, Headers, BodyBin, Options) of
        {ok, 200, _, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RetBodyBin} ->
                    % 显式关闭连接
                    hackney:close(ClientRef),
                    case catch erlang_docker_misc:json_decode(RetBodyBin) of
                        #{code := 0, data := Data} ->
                            {ok, Data};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
```

## 镜像

镜像的配置, 打包和上传, 其中上传的镜像仓库使用本地搭建的 `registry:2` 仓库

### Dockerfile

这里的基础镜像使用 `erlang:25.3.2.8-alpine`, 将代码拷贝到镜像里进行打包, 保存下发配置到 `Dockerfile` 文件

```dockerfile
FROM erlang:25.3.2.8-alpine

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

```

> 启动服务时, 必须以 `foreground` 而不是 `daemon` 启动, 因为得让节点在前台运行, 否则容器执行完命令就停止了

### 打包镜像

执行:

```bash
make build_a && make build_b
```

会分别打包当前 `erlang` 代码的镜像, 并保存到 `./docker_images` 目录

- erlang-docker-a.tar
- erlang-docker-b.tar

### 加载镜像

先把镜像上传到 `Master` 虚拟机

```bash
make scp_images
```

完成后, 进入 `Master` 虚拟机应该就能看到这两个镜像了, 把镜像加载到 `docker`, 接下来都以 `erlang-docker-a.tar` 为例

```bash
ssh master
sudo docker load -i ./erlang-docker-a.tar
```

### 本地镜像仓库

由于 `K8S` 部署 `pod` 时, 会去 `docker.io` 上去拉取镜像, 但我们现在用的是本地打包的镜像, 私有镜像也不能上传到外网, 所以就需要搭建本地的镜像地址

根据当前 `Master` 虚拟机 `ip`, 添加配置, 这里以我的 `Master` 为例

```bash
ssh master
sudo vi /etc/docker/daemon.json
```

加入内容并保存

```json
{
  "insecure-registries": ["http://{MasterIP}:5000"]
}
```

停止并删除现有的 `Docker Registry` 容器

```bash
sudo docker stop registry
sudo docker rm registry
sudo docker rmi registry:2
```

安装 `registry:2` 镜像并启动服务

```bash
sudo docker run -d -p 5000:5000 --name registry registry:2
```

等待几秒钟后, 验证服务启动

```bash
curl http://{MasterIP}:5000/v2/
```

你应该得到如下的响应：

```bash
{}
```

如果不是, 可以尝试重启服务

```bash
sudo systemctl daemon-reload
sudo systemctl restart docker
```

还不行则删除 `registry:2` 重新安装

```bash
sudo docker stop registry
sudo docker rm registry
sudo docker rmi registry:2
sudo docker run -d -p 5000:5000 --name registry registry:2
# 等待几秒钟后
curl http://{MasterIP}:5000/v2/
```

然后每个 `Worker` 虚拟机也都做相同操作, 需要注意的是, `Worker` 的 `/etc/docker/daemon.json` 里面也是用 `Master` 的 `IP` 地址

### 上传镜像

将镜像上传到本地仓库 `registry:2`, 先标记我们自己打包的镜像

```bash
sudo docker tag erlang-docker-a:latest {MasterIP}:5000/erlang-docker-a:latest
```

这时候可以看到两个 `erlang-docker-a`, 一个是一开始 `docker load` 的, 一个是刚刚打了 `tag` 的

```bash
sudo docker images
```

你应该得到如下的响应：

```bash
REPOSITORY                           TAG                    IMAGE ID       CREATED         SIZE
{MasterIP}:5000/erlang-docker-a      latest                 eb58965162c3   15 hours ago    103MB
erlang-docker-a                      latest                 eb58965162c3   15 hours ago    103MB
```

推送镜像到本地镜像仓库

```bash
sudo docker push {MasterIP}:5000/erlang-docker-a:latest
```

尝试拉取镜像

```bash
sudo docker pull {MasterIP}:5000/erlang-docker-a:latest
```

## 配置目录

创建目录 `k8s_yaml`, `erlang-docker-a` 和 `erlang-docker-b`, 及后面添加的配置名字和目录结构如下:

```bash
├── k8s_yaml
│   ├── erlang-docker-a
│   │   ├── erlang-docker-a-headless.yaml
│   │   ├── erlang-docker-a-service.yaml
│   │   └── erlang-docker-a-statefulset.yaml
│   ├── erlang-docker-b
│   │   ├── erlang-docker-b-headless.yaml
│   │   ├── erlang-docker-b-service.yaml
│   │   └── erlang-docker-b-statefulset.yaml
│   └── erlang-docker-storageclass.yaml
```

## StorageClass 配置

同一种服务的多个 `pod` 需要共用一块存储位置来保存日志或 `mnesia`, 且在 `pod` 销毁后卷要保留不能销毁, 所以需要配置申请卷

申请卷需要有 `StorageClass` 配置, 这里使用本地配置, 保存下方配置为: `erlang-docker-storageclass.yaml`

```yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: local-storage
provisioner: kubernetes.io/no-provisioner   # 本地存储
volumeBindingMode: WaitForFirstConsumer

```

`provisioner` 这里指定了用本地存储

## a版节点配置

同一套代码但模拟两个不同类型的 `erlang` 节点, 配置成两套服务

### Service 配置

将 `service` 的 `type` 配置为 `NodePort`, 让其暴露在集群外部, 但它不具备负载均衡, 同时推荐只在开发环境使用

保存下方配置为: `erlang-docker-a-service.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlang-docker-a-svc
  namespace: default
  labels:
    app: erlang-docker-a
spec:
  type: NodePort            # NodePort 不能负载均衡, 若要负载均衡, 需要配置为 LoadBalancer, 然后使用 ingress 组件 + Nginx 做负载均衡
  selector:
    app: erlang-docker-a
  ports:
    - name: http            # 用于 service http 访问时用
      protocol: TCP         # 协议类型, TCP
      port: 8080            # 集群内 http 访问端口
      targetPort: 8080      # 对应 `pod` 中程序的端口, 这里是 erlang 节点的 http 端口
      nodePort: 30080       # 映射到外部服务时, 节点的端口
    - name: ws              # 用于 service ws 访问时用
      protocol: TCP         # 协议类型, TCP
      port: 9090            # 集群内 ws 访问端口
      targetPort: 9090      # 对应 `pod` 中程序的端口, 这里是 erlang 节点的 ws 端口
      nodePort: 30090       # 映射到外部服务时, 节点的端口

```

`spec.selector.app` 配置需要跟 `StatefulSet` 配置的 `spec.template.metadata.labels` 对应上

### Headless Service 配置

使用 `headless` 类型的 `service` 配置, 目的是搭配 `statefulset` 配置, 让 `pod` 有唯一的 `DNS` 名字, 从而用于 `erlang` 节点的命名, 让 `pod` 重启后依然有唯一的 `erlang` 节点名字

保存下方配置为: `erlang-docker-a-headless.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlang-docker-a-hl
  namespace: default
  labels:
    app: erlang-docker-a
spec:
  clusterIP: None
  selector:
    app: erlang-docker-a
  ports:
    - port: 80              # 用于 service 访问时用
      targetPort: 8080      # 对应 `pod` 中程序的端口, 这里是 erlang 节点的 http 端口

```

### StatefulSet 配置

因为 `erlang` 节点需要用到日志和 `mnesia` 保存, 且是有状态的服务, 所以需要使用 `StatefulSet` 而不是 `Deployment`

保存下方配置为: `erlang-docker-a-statefulset.yaml`

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlang-docker-a-sfs
  namespace: default
spec:
  serviceName: "erlang-docker-a-hl"                     # 需要跟某个 `svc` 配置的 `metadata.name` 对应, 由于需要生成唯一的 DNS, 所以这里使用 `headless`` 配置的名字
  replicas: 3                                           # pod 容器数量
  selector:
    matchLabels:
      app: erlang-docker-a                              # 必须匹配 .spec.template.metadata.labels
  template:
    metadata:
      labels:
        app: erlang-docker-a                            # 必须匹配 .spec.selector.matchLabels
    spec:
      containers:
        - name: erlang-docker-a
          image: {MasterIP}:5000/erlang-docker-a:latest
          env:
            - name: NAMESPACE                           # 命名空间
              valueFrom:
                fieldRef:
                  fieldPath: metadata.namespace
            - name: POD_NAME                            # pod 名字
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name
            - name: LOCAL_IP                            # 节点IP
              valueFrom:
                fieldRef:
                  fieldPath: status.podIP
            - name: ERLANG_HOST_NAME                    # 节点 DNS 名字
              value: "$(POD_NAME).erlang-docker-a-hl"   # 完整名字 $(POD_NAME).erlang-docker-a-hl.$(NAMESPACE).svc.cluster.local
            - name: A_SERVICE_DOMAIN                    # a 版服务 service 集群内部域名
              value: "erlang-docker-a-svc"              # 完整名字 erlang-docker-a-svc.$(NAMESPACE).svc.cluster.local
            - name: B_SERVICE_DOMAIN                    # b 版服务 service 集群内部域名
              value: "erlang-docker-b-svc"              # 完整名字 erlang-docker-b-svc.$(NAMESPACE).svc.cluster.local
          ports:
            - containerPort: 4369                       # erlang 节点 epmd 端口
              name: epmd
            - containerPort: 8080                       # erlang 节点 http 端口
              name: http
            - containerPort: 9090                       # erlang 节点 ws 端口
              name: ws
          volumeMounts:
            - name: erlang-docker-a-volume              # 使用的持久卷名称
              mountPath: /mnt/erlang-docker-a-volume    # 将申请的卷挂载到容器的 /mnt/erlang-docker-a-volume 目录
  volumeClaimTemplates:
    - metadata:
        name: erlang-docker-a-volume                    # 持久卷名称
      spec:
        accessModes: [ "ReadWriteOncePod" ]             # 访问模式, ReadWriteOncePod: 单个pod读写
        resources:
          requests:
            storage: 1Gi                                # 申请的持久卷大小, 这里是 1GB

```

- `spec.replicas: 1` 指定了部署的 `pod` 数量为 1
- `spce.serviceName` 需要使用某个 `service` 配置的 `metadata.name` 对应才能通过该 `service` 生成对应的 `DNS`
- `spec.selector.matchLabels` 名字必须与 `spec.template.metadata.labels` 一致
- `spec.template.spec.containers` 用于配置容器的镜像, 可配置多个, 但一般一个容器只运行一个镜像
- `spec.template.spec.containers.image` 指定容器镜像地址, 用于部署时拉取, 自行将 `{MasterIP}` 替换为当前 `master` 虚拟机 `ip`
- `volumeClaimTemplates` 申请卷, 需要搭配 `StorageClass` 配置

## b版节点配置

跟上面 a版配置基本一致

### Service 配置

保存下方配置为: `erlang-docker-b-service.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlang-docker-b-svc
  namespace: default
  labels:
    app: erlang-docker-b
spec:
  type: ClusterIP
  selector:
    app: erlang-docker-b
  ports:
    - port: 8080            # 用于 service http 访问时用
      targetPort: 8080      # 对应 `pod` 中程序的端口, 这里是 erlang 节点的 http 端口

```

### Headless Service 配置

保存下方配置为: `erlang-docker-b-headless.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlang-docker-b-hl
  namespace: default
  labels:
    app: erlang-docker-b
spec:
  clusterIP: None
  selector:
    app: erlang-docker-b
  ports:
    - port: 80              # 用于 service 访问时用
      targetPort: 8080      # 对应 `pod` 中程序的端口, 这里是 erlang 节点的 http 端口

```

### StatefulSet 配置

保存下方配置为: `erlang-docker-b-statefulset.yaml`

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlang-docker-b-sfs
  namespace: default
spec:
  serviceName: "erlang-docker-b-hl"                     # 需要跟某个 `svc` 配置的 `metadata.name` 对应, 由于需要生成唯一的 DNS, 所以这里使用 `headless`` 配置的名字
  replicas: 3                                           # pod 容器数量
  selector:
    matchLabels:
      app: erlang-docker-b                              # 必须匹配 .spec.template.metadata.labels
  template:
    metadata:
      labels:
        app: erlang-docker-b                            # 必须匹配 .spec.selector.matchLabels
    spec:
      containers:
        - name: erlang-docker-b
          image: {MasterIP}:5000/erlang-docker-b:latest
          env:
            - name: NAMESPACE                           # 命名空间
              valueFrom:
                fieldRef:
                  fieldPath: metadata.namespace
            - name: POD_NAME                            # pod 名字
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name
            - name: LOCAL_IP                            # 节点IP
              valueFrom:
                fieldRef:
                  fieldPath: status.podIP
            - name: ERLANG_HOST_NAME                    # 节点 DNS 名字
              value: "$(POD_NAME).erlang-docker-b-hl"   # 完整名字 $(POD_NAME).erlang-docker-b-hl.$(NAMESPACE).svc.cluster.local
            - name: A_SERVICE_DOMAIN                    # a 版服务 service 集群内部域名
              value: "erlang-docker-a-svc"              # 完整名字 erlang-docker-a-svc.$(NAMESPACE).svc.cluster.local
            - name: B_SERVICE_DOMAIN                    # b 版服务 service 集群内部域名
              value: "erlang-docker-b-svc"              # 完整名字 erlang-docker-b-svc.$(NAMESPACE).svc.cluster.local
          ports:
            - containerPort: 4369                       # erlang 节点 epmd 端口
              name: epmd
            - containerPort: 8080                       # erlang 节点 http 端口
              name: http
            - containerPort: 9090                       # erlang 节点 ws 端口
              name: ws
          volumeMounts:
            - name: erlang-docker-b-volume              # 使用的持久卷名称
              mountPath: /mnt/erlang-docker-b-volume    # 将申请的卷挂载到容器的 /mnt/erlang-docker-b-volume 目录
  volumeClaimTemplates:
    - metadata:
        name: erlang-docker-b-volume                    # 持久卷名称
      spec:
        accessModes: [ "ReadWriteOncePod" ]             # 访问模式, ReadWriteOncePod: 单个pod读写
        resources:
          requests:
            storage: 1Gi                                # 申请的持久卷大小, 这里是 1GB

```

## 应用配置

将 `.yaml` 配置都发送到 `Master` 虚拟机

```bash
make scp_yaml
```

进入 `Master` 虚拟机, 应用 `.yaml` 配置

```bash
ssh master
# 应用 a版配置
sudo kubectl apply -f ./k8s_yaml/erlang-docker-a/erlang-docker-a-service.yaml
sudo kubectl apply -f ./k8s_yaml/erlang-docker-a/erlang-docker-a-headless.yaml
sudo kubectl apply -f ./k8s_yaml/erlang-docker-a/erlang-docker-a-statefulset.yaml
# 应用 b版配置
sudo kubectl apply -f ./k8s_yaml/erlang-docker-b/erlang-docker-b-service.yaml
sudo kubectl apply -f ./k8s_yaml/erlang-docker-b/erlang-docker-b-headless.yaml
sudo kubectl apply -f ./k8s_yaml/erlang-docker-b/erlang-docker-b-statefulset.yaml
```

过一会查看服务

```bash
sudo kubectl get all -o wide
```

你应该得到如下的响应：

```bash
NAME                        READY   STATUS    RESTARTS   AGE     IP           NODE      NOMINATED NODE   READINESS GATES
pod/erlang-docker-a-sfs-0   1/1     Running   0          3m16s   10.42.1.26   worker1   <none>           <none>
pod/erlang-docker-a-sfs-1   1/1     Running   0          2m35s   10.42.3.20   worker3   <none>           <none>
pod/erlang-docker-a-sfs-2   1/1     Running   0          113s    10.42.2.25   worker2   <none>           <none>
pod/erlang-docker-b-sfs-0   1/1     Running   0          2m32s   10.42.2.24   worker2   <none>           <none>
pod/erlang-docker-b-sfs-1   1/1     Running   0          109s    10.42.3.21   worker3   <none>           <none>
pod/erlang-docker-b-sfs-2   1/1     Running   0          51s     10.42.1.27   worker1   <none>           <none>

NAME                          TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)                       AGE     SELECTOR
service/kubernetes            ClusterIP   10.43.0.1       <none>        443/TCP                       28h     <none>
service/erlang-docker-a-svc   NodePort    10.43.27.43     <none>        80:30080/TCP,9090:30090/TCP   3m17s   app=erlang-docker-a
service/erlang-docker-a-hl    ClusterIP   None            <none>        80/TCP                        3m17s   app=erlang-docker-a
service/erlang-docker-b-svc   ClusterIP   10.43.242.177   <none>        80/TCP                        2m33s   app=erlang-docker-b
service/erlang-docker-b-hl    ClusterIP   None            <none>        80/TCP                        2m32s   app=erlang-docker-b

NAME                                   READY   AGE     CONTAINERS        IMAGES
statefulset.apps/erlang-docker-a-sfs   3/3     3m16s   erlang-docker-a   {MasterIP}/erlang-docker-a:latest
statefulset.apps/erlang-docker-b-sfs   3/3     2m32s   erlang-docker-b   {MasterIP}/erlang-docker-b:latest
```

## 测试

### 打包并发布

所有代码和配置准备好后, 可以直接执行 `make`, 会自动打包 `docker` 镜像, 然后把镜像压缩包, 配置文件 和 更新脚本 一起发送到 `master` 虚拟机

```bash
# ALL: build_a build_b scp
make
```

执行完 `make` 后, 进入 `master` 虚拟机, 为了方便更新节点镜像和 `k8s` 部署, 执行脚本即可

```bash
ssh master
# 需要以 `root` 执行脚本
sudo ./update.sh
```

如果虚拟机经过多次更新镜像后, 磁盘剩余空间越来越少, 可以在本地执行

```bash
make worker_clean
```

### http

ping 服务器

```bash
curl http://{MasterIP}:30080/ping
```

获取节点

```bash
curl -d "{}" http://{MasterIP}:30080/node
```

### websocket

浏览器打开 `http://{MasterIP}:30090`

- 点击 `connection` 会切换连接 或 关闭 `ws`, 这里点击按钮连接 `ws`
- `send` 按钮会把相邻输入框的文本发送到服务器
- `node` 按钮会自动填写 "node" 字符串到输入框并自动发送, 会把当前节点返回到界面, 这里应该是 a版节点
- `mfa` 按钮会把相邻输入框的 `erlang` 接口发送到服务器执行并返回结果, 这里应该是在 a版节点执行
- `rpc mfa` 按钮会把相邻输入框的 `erlang` 接口发送到服务器, 然后 `rpc` 到 b版节点执行再返回结果

## 异常与解决方法

### 镜像启动
   
`Dockerfile` 启动 `erlang` 节点时, 必须以 `foreground` 而不是 `daemon` 启动, 因为得让节点在前台运行, 否则容器执行完命令就停止了

### Pod 的 DNS

使用容器的 `DNS` 有助于 `erlang` 将节点命名为固定且唯一的名字, 而 `pod` 要使用某个 `service` 的 `DNS`, 则 `statefulset` 配置的 `spce.serviceName` 需要使用对应 `service` 的 `metadata.name`, 这样才能通过该 `service` 生成对应的 `DNS`

比如:

配置为 `service` 的 `metadata.name` 时, `pod` 的 `DNS` 为: 

```bash
erlang-docker-a-sfs-xxx.erlang-docker-a-svc.default.svc.cluster.local
# 可简写为
erlang-docker-a-sfs-xxx.erlang-docker-a-svc
```

配置为 `headless service` 的 `metadata.name` 时, `pod` 的 `DNS` 为: 

```bash
erlang-docker-a-sfs-xxx.erlang-docker-a-hl.default.svc.cluster.local
# 可简写为
erlang-docker-a-sfs-xxx.erlang-docker-a-hl
```

### 节点唯一名字

由于要考虑 `mnesia` 会保存 `erlang` 节点名字, 当用以往的方式创建 `erlang` 节点时, 节点名字是 `名字@ip地址`, 但 `pod` 一旦重启就会使用新的 `ip` 地址, 这时 `erlang` 节点名字就会改变, 而它去读取 `mnesia` 时, 与保存了的节点名字会对应不上从而报错, 这就需要有个唯一的 `erlang` 节点名字, 无论 `pod` 重启多少次都不会改变

解放方法是将 `erlang` 节点名字改为用 `DNS` 的方式命名, 要先将 `DNS` 配置到环境变量供 `vm.args` 读取, 如:

```yaml
# 命名空间, 一般为 default, 这里获取后设置为环境变量, 是为了后续拼接名字时预防写死而对应不上
- name: NAMESPACE
  valueFrom:
    fieldRef:
      fieldPath: metadata.namespace
# pod 名字, 根据 容器名字-PodId 拼接成, 如: erlang-docker-a-sfs-0, erlang-docker-a-sfs-1 等
- name: POD_NAME
  valueFrom:
    fieldRef:
      fieldPath: metadata.name
# erlang 节点 host 名字, 也是 pod 的 DNS 名字, 格式: Erlang项目名字@Pod名字.SVC名字.命名空间.svc.cluster.local
- name: ERLANG_HOST_NAME
  value: "$(POD_NAME).erlang-docker-a-hl.$(NAMESPACE).svc.cluster.local"
  # 或可简写为
  value: "$(POD_NAME).erlang-docker-a-hl"
```

然后将 `erlang` 节点名字配置为:

```args
# config/a_vm.args.src
-name erlang-docker-a@${ERLANG_HOST_NAME}
# config/b_vm.args.src
-name erlang-docker-b@${ERLANG_HOST_NAME}
```

而唯一的 `DNS` 需要使用 `headless service` 和 `statefulset` 配置搭配来启动容器

### 集群内部 `service` 域名地址

`erlang` 节点之间在还未得到目标节点名字时要进行 `rpc`, 以前做法是通过缓存的多个节点随机出一个进行 `rpc`, 但既然用了 `K8S`, 那就需要通过负载均衡获取才有意义, 而通过负载均衡请求 `erlang` 节点就需要知道 `service` 的域名或地址

这里使用 `statefulset` 配置时, 在 `env` 写死域名地址

```yaml
- name: A_SERVICE_DOMAIN
  value: "erlang-docker-a-svc"
- name: B_SERVICE_DOMAIN
  value: "erlang-docker-b-svc"
```

同时 `erlang` 配置获取这两个环境变量, 让 `erlang` 节点读取并用于 `http` 负载均衡请求

```config
[
    {erlang_docker, [
        {http_port, 8080},
        {ws_port, 9090},
        {a_svc_domain, "${A_SERVICE_DOMAIN}"},
        {b_svc_domain, "${B_SERVICE_DOMAIN}"}
    ]}
].
```

### 反复请求 `service` 接口负载均衡

如果按照通常做法去请求 `service` 的接口, 会服用连接, 导致每次返回的结果都是一样的, 而不是经过负载均衡返回的结果

所以每次要负载均衡的时候, 关闭连接池, 并显示关闭链接, 让下次请求是创建新的链接而不是复用链接

```erlang
%% @doc http 请求
request(Method, Url, Router, Body) ->
    NewUrl = Url ++ Router,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    BodyBin = jsx:encode(Body),
    % {pool, false}: 禁用连接池确保每次请求都不复用连接, 否则复用会导致无法负载均衡
    Options = [{pool, false}, {recv_timeout, 5000}],
    case catch hackney:request(Method, NewUrl, Headers, BodyBin, Options) of
        {ok, 200, _, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RetBodyBin} ->
                    % 显式关闭连接
                    hackney:close(ClientRef),
                    case catch erlang_docker_misc:json_decode(RetBodyBin) of
                        #{code := 0, data := Data} ->
                            {ok, Data};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
```
