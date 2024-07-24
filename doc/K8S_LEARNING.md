# K8S Learning <!-- omit in toc -->

- [一、前言](#一前言)
- [二、组件概念](#二组件概念)
  - [1. node](#1-node)
  - [2. pod](#2-pod)
  - [3. svc](#3-svc)
  - [4. ing](#4-ing)
  - [5. cm](#5-cm)
  - [6. secret](#6-secret)
  - [7. vol](#7-vol)
  - [8. deploy](#8-deploy)
  - [9. sts](#9-sts)
- [三、架构概念](#三架构概念)
  - [1. worker node](#1-worker-node)
    - [1) container-runtime](#1-container-runtime)
    - [2) kubelet](#2-kubelet)
    - [3) k-proxy](#3-k-proxy)
  - [2. master node](#2-master-node)
    - [1) api](#1-api)
    - [2) sched](#2-sched)
    - [3) c-m](#3-c-m)
    - [4) etcd](#4-etcd)
    - [5) c-c-m](#5-c-c-m)
- [四、工具](#四工具)
  - [1. kubectl](#1-kubectl)
  - [2. minikube](#2-minikube)
  - [3. K3S + Multipass](#3-k3s--multipass)
    - [1) 安装 multipass](#1-安装-multipass)
    - [2) 常用命令](#2-常用命令)
    - [3) 创建 Master 虚拟机](#3-创建-master-虚拟机)
    - [4) 使用 ssh 登录虚拟机](#4-使用-ssh-登录虚拟机)
    - [5) 安装 k3s](#5-安装-k3s)
    - [6) 获取 Token](#6-获取-token)
    - [7) 创建 Worker 虚拟机](#7-创建-worker-虚拟机)
    - [8) 查看集群信息](#8-查看集群信息)
- [五、kubectl 常用命令](#五kubectl-常用命令)
  - [1. 查看 nodes](#1-查看-nodes)
  - [2. 查看 svc](#2-查看-svc)
  - [3. 查看 pod](#3-查看-pod)
  - [4. 创建 pod](#4-创建-pod)
  - [5. create 命令](#5-create-命令)
  - [6. 创建 deployment](#6-创建-deployment)
  - [7. 创建 service](#7-创建-service)
  - [8. 修改 deployment](#8-修改-deployment)
  - [9. 查看日志](#9-查看日志)
  - [10. 进入容器终端](#10-进入容器终端)
  - [11. 查看所有资源](#11-查看所有资源)
  - [12. 删除资源](#12-删除资源)
- [六、配置文件](#六配置文件)
  - [1. deployment 配置](#1-deployment-配置)
    - [1) apiVersion](#1-apiversion)
    - [2) kind](#2-kind)
    - [3) metadata](#3-metadata)
    - [4) spec](#4-spec)
    - [5) 使用配置文件创建资源对象](#5-使用配置文件创建资源对象)
    - [6) 使用配置文件删除资源对象](#6-使用配置文件删除资源对象)
    - [7) 应用配置文件](#7-应用配置文件)
  - [2. service 配置](#2-service-配置)
    - [1) apiVersion](#1-apiversion-1)
    - [2) kind](#2-kind-1)
    - [3) metadata](#3-metadata-1)
    - [4) spec](#4-spec-1)
    - [5) 应用配置文件](#5-应用配置文件)
- [七、配置和公开服务](#七配置和公开服务)
  - [1. 公开服务](#1-公开服务)
  - [2. 查看服务描述](#2-查看服务描述)
  - [3. 删除服务](#3-删除服务)
  - [4. NodePort](#4-nodeport)

## 一、前言

- `K8S` 即 `Kubernetes`
- `Docker` 只是 `K8S` 众多容器实现中的一种

## 二、组件概念

`K8S` 有很多组件, 但常用的一般有 9 个

### 1. node

- 节点, 可以是物理机或虚拟机
- 一个 `node` 可以运行一个或多个 `pod`

### 2. pod

- `pod` 是 `K8S` 最小的调度单元, 他提供了运行容器的环境
- 一个 `pod` 就是一个或多个应用容器的组合
- 通常一个应用程序放在一个 `pod`, 建议一个 `pod` 运行一个容器
- 每个 `pod` 在创建时会自动生成一个 `ip` 地址, 该 `ip` 地址在集群外部无法被访问
- `pod` 发生故障时被销毁后, 创建的新 `pod` 也会生成新的 `ip` 地址

### 3. svc

- 即 `service`, 可以将一组 `pod` 封装成一个 `svc`
- 要连接某个 `pod` 是通过访问这个 `svc` 的 `ip` 来连接的
- 某个 `svc` 下的 `pod` 异常被销毁, 消息会被转发到其他健康的 `pod`
- 用于内部或没必要暴露的 `svc` 如 应用程序和数据库的连接 `svc`, 称为 "内部服务"
- 用于暴露在外部连接 `svc` 如客户端连接服务器的 `svc`, 称为 "外部服务"
- 在开发环境和测试环境可以使用 `svc` 作为外部服务, 但在生产环境为了避免暴露 `ip` 和 `port`, 则需要使用 `ingress` 组件改为用域名来访问

### 4. ing

- 即 `ingress`, 用来管理集群外部访问集群内部服务的入口和方式的组件
- 通过配置不同的转发规则, 从而根据不同规则来访问集群不同的 `svc` 和 `pod`
- 将原本访问 `svc` 的 `ip` 和 `port` 转换成域名来访问
- 可以负责负载均衡, ssl 证书配置等

### 5. cm

- 即 `config map`, 用于保存镜像的配置信息
- 配置信息是明文的
- 比如某个镜像需要访问数据库, 可以将用户名和密码保存到 `cm` 组件来获取
- 如果用户名或密码修改了, 无需重新打包镜像重新部署, 只需修改 `cm` 配置, 然后重新加载 `pod` 即可, 无需重新编译和部署
- 但通常这种方式要避免保存像用户名和密码这样的敏感信息

### 6. secret

- 与 `cm` 类似, 可以将敏感配置信息封装起来
- 配置信息不是明文的, 但依然不安全, 因为只用了一层 `base64` 编码而已
- 通过其他组件增加安全性, 如 `user`(网络安全), `c.role`(访问控制)和`sa`(身份认证)

### 7. vol

- 即 `volume`, 可以将需要持久化的数据挂载到集群的本地磁盘或集群外的远程存储

像数据库这类的数据就不怕容器被销毁或重启了

### 8. deploy

- 即 `deployment`, 用于部署无状态的应用程序, 如 `http` 服务
- 简化应用程序的部署和更新操作
- 可以 副本控制, 滚动更新, 自动扩缩等
- 副本控制: 定义和管理应用程序的副本数量
- 滚动更新: 应用程序的更新策略, 逐渐使用新的版本替换旧版本
- 自动扩缩: 根据 `pod` 的 `CPU` 使用率或其他自定义指标自动调整副本数量
- 持久化数据应用程序一般不建议使用该功能, 有丢数据风险, 可以使用 `sts` 代替

### 9. sts

- 即 `statefulset`, 跟 `deploy` 相似, 但用于部署有状态的应用程序
- 可以 副本控制, 自动扩缩等
- 保证每个副本都有自己稳定的网络标识符和持久化存储
- 数据库, `redis`, `mq` 或 有状态的服务 如 `Erlang` 服务都适合用该组件部署

`Erlang` 节点之所以需要使用 `sts` 部署, 是因为需要使用动态生成的唯一 `DNS` 拼接成 `Erlang` 节点名字, 这样才能绕过负载均衡而正确进行 `rpc` 通信

> 部署过程繁琐复杂, 对于部署数据库这类服务时, 更通用和简单的方式是将这些服务从 `K8S` 集群剥离开来, 在集群外单独部署

## 三、架构概念

`Master-Worker` 架构, `Master-Node` 负责管理整个集群, `Worker-Node` 负责运行应用程序和服务

### 1. worker node

为了提供对外服务, 每个 `worker-node` 都会包含 3 个核心组件

#### 1) container-runtime

- 即 `容器运行时`, 负责拉取镜像, 创建容器, 启动或停止容器等
- 常见的容器运行时有 `Docker-Engine`, `Containerd`, `CRI-O` 和 `Mirantis Container Runtime`

#### 2) kubelet

- 负责管理和维护 `node` 上的 `pod`, 确保他们按照预期运行
- 定期从 `api-server` 组件接收新的或修改后的 `pod` 规范
- 监控 `node` 运行情况, 并汇报给 `api-server`

#### 3) k-proxy

- 即 `kube-proxy`, 负责为 `pod` 提供网络代理和负载均衡服务
- 通常集群包含多个 `node`, 这些 `node` 通过 `svc` 进行通信, 这就需要 `kube-proxy` 接收请求, 在 `node` 启动一个网络代理, 将消息发往 `pod`

### 2. master node

管理多个 `worker-node`, 每个 `master-node` 都会包含 4 个基本组件

#### 1) api

- 即 `kube-apiserver`, 负责 `K8S` 集群的 `API` 接口服务
- 所有组件都会通过这个接口来通信
- 就像整个 `K8S` 集群的网关, 是整个系统的入口
- 同时对所有资源对象提供 增删改查 等操作进行 认证, 授权 和 访问控制
- 使用 `kubectl` 命令行 或 `Dashboard` 等其他图形化 `UI` 工具来交互

#### 2) sched

- 即 `scheduler` 调度器, 负责监控整个集群的节点资源使用情况
- 根据一些调度策略, 将 `pod` 调度到合适的节点运行

#### 3) c-m

- 即 `controller manager` 控制器管理器, 监控集群中各种资源服务的状态
- 包括 `node`, `pod`, `svc` 等
- 如 检测到集群的某个 `node` 的某个 `pod` 发生故障时, 重启或创建新的 `pod`
- 使用 `etcd` 来获取各种资源的状态

#### 4) etcd

- 高可用的 键-值 存储系统
- 存储集群所有资源的状态信息
- 当使用 `kubectl` 命令行 或 其他 `UI` 工具查询集群状态时, 就是从 `etcd` 获取的 

#### 5) c-c-m

- 即 `cloud controller manager`, 云平台的控制器管理器
- 有 `Google GKE`, `Microsoft AKS`, `Amazon EKS`

## 四、工具

### 1. kubectl

用于 `K8S` 交互的命令行工具

```bash
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/darwin/amd64/kubectl"
chmod +x ./kubectl
sudo mv ./kubectl /usr/local/bin/kubectl
sudo chown root: /usr/local/bin/kubectl
```

### 2. minikube

`K8S` 集群的一种实现, 可以在本地搭建和运行简版的 `K8S` 单节点的集群环境, 以便在本地学习, 开发和测试集群服务

```bash
brew install minikube # 安装 minikube
minikube version      # 查看版本
minikube              # 列出帮助
minikube start        # 启动集群
minikube stop         # 停止集群
minikube status       # 集群状态
minikube delete       # 删除集群
```

### 3. K3S + Multipass

`minikube` 只是单节点集群环境, 要多节点就需要使用 `K3S` + `Multipass` 来搭建

#### 1) 安装 multipass

```bash
brew install multipass
```

#### 2) 常用命令

```bash
multipass launch --name xxx # 创建虚拟机, 命名为 xxx
multipass start xxx         # 启动名字为 xxx 的虚拟机
multipass stop xxx          # 停止名字为 xxx 的虚拟机
multipass shell xxx         # 进入名字为 xxx 的虚拟机
multipass exec xxx -- ls -l # 在名字为 xxx 的虚拟机执行命令 `ls -l`
multipass list              # 查看虚拟机列表
multipass help              # 帮助
```

安装完后, 使用 `multipass` 在本地安装 3 个虚拟机, 每个虚拟机里安装 `K3S`, 也就是 3 个节点, 其中一个是 `master-node`, 另外两个是 `worker-node`

#### 3) 创建 Master 虚拟机

命名为 k3s, Cpu 2核, 内存 8G, 硬盘 10G, `multipass` 会自动下载 `Ubuntu` 镜像创建虚拟机

```bash
multipass launch --name k3s --cpus 2 --memory 8G --disk 10G
multipass list
```

#### 4) 使用 ssh 登录虚拟机

本地 `shell` 生成公钥

```bash
sh-keygen -t rsa -b 4096
cat .ssh/id_rsa.pub
```

`cat` 出来后复制粘贴到虚拟机

```bash
multipass shell k3s
vi .ssh/authorized_keys   # 打开并在新一行粘贴, 保存
sudo passwd ubuntu        # 修改 ubuntu 用户密码
sudo apt update
sudo apt upgrade
exit
```

回到本地 `shell`, 打开 `~/.ssh/config` 并添加

```config
Host *
    StrictHostKeyChecking no

Host master
    HostName 虚拟机Master的ip
    User ubuntu
```

保存后, 直接 `ssh master` 即可连接该 `Master` 虚拟机

#### 5) 安装 k3s

[k3s 官网](https://k3s.io)

虚拟机里安装 `k3s`

```bash
sudo curl -sfL https://get.k3s.io | sh -
```

安装后使用 `kubectl` 查看节点, 名字是 `master`, 状态是 `Ready`

```bash
~$ sudo kubectl get nodes
NAME   STATUS   ROLES                  AGE   VERSION
k3s    Ready    control-plane,master   14m   v1.29.5+k3s1
```

#### 6) 获取 Token

该 `Token` 是让其他节点加入的凭证

```bash
sudo cat /var/lib/rancher/k3s/server/node-token
```

打开新的本地终端, 将 `Token` 保存到本地环境变量里

```bash
TOKEN=$(multipass exec k3s sudo cat /var/lib/rancher/k3s/server/node-token)
echo $TOKEN
```

#### 7) 创建 Worker 虚拟机

获取 `master` 节点 `ip` 并保存到本地环境变量

```bash
MASTER_IP=$(multipass info k3s | grep IPv4 | awk '{print $2}')
echo $MASTER_IP
```

创建 3 个 `worker` 节点

```bash
multipass launch --name worker1  --cpus 2 --memory 8G --disk 10G
multipass launch --name worker2  --cpus 2 --memory 8G --disk 10G
multipass launch --name worker3  --cpus 2 --memory 8G --disk 10G
```

打开 `~/.ssh/config` 并添加

```config
Host worker1
    HostName 虚拟机Woker1的IP
    User ubuntu

Host worker2
    HostName 虚拟机Woker2的IP
    User ubuntu

Host worker3
    HostName 虚拟机Woker3的IP
    User ubuntu
```

保存后, 直接 `ssh worker1` 或 `ssh worker2` 即可连接到每台 `Worker` 虚拟机

在两个 `woker` 虚拟机安装 `k3s` 并加入到集群中, 本地执行

```bash
for f in 1 2 3 ; do
    multipass exec worker$f -- bash -c "curl -sfL https://get.k3s.io | K3S_URL=\"https://$MASTER_IP:6443\" K3S_TOKEN=\"$TOKEN\" sh -"
done
```

#### 8) 查看集群信息

运行完成后, 进入 `master` 虚拟机, 可以看到两个 `worker` 节点注册到了集群中

```bash
~$ ssh master
~$ sudo kubectl get nodes
NAME      STATUS   ROLES                  AGE     VERSION
k3s       Ready    control-plane,master   59m     v1.29.5+k3s1
worker1   Ready    <none>                 2m28s   v1.29.5+k3s1
worker2   Ready    <none>                 2m9s    v1.29.5+k3s1
worker3   Ready    <none>                 1m3s    v1.29.5+k3s1
```

## 五、kubectl 常用命令

在安装好上面的环境后, 这里选择 `k3s`, 进入 `master` 节点

```bash
ssh master
```

### 1. 查看 nodes

查看所有的节点

```bash
~$ sudo kubectl get nodes
NAME      STATUS   ROLES                  AGE   VERSION
k3s       Ready    control-plane,master   71m   v1.29.5+k3s1
worker1   Ready    <none>                 13m   v1.29.5+k3s1
worker2   Ready    <none>                 13m   v1.29.5+k3s1
worker3   Ready    <none>                 13m   v1.29.5+k3s1
```

### 2. 查看 svc

查看所有的服务, `kubernetes` 服务是默认服务, 安装好集群环境后会自动创建

```bash
~$ sudo kubectl get svc
NAME         TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   72m
```

### 3. 查看 pod

查看所有的 `pod`, 这里还没有创建所以说空的

```bash
~$ sudo kubectl get pod
No resources found in default namespace.
```

### 4. 创建 pod

如 创建 `Nginx` 的 `pod`, 这样查看 `pod` 就能看到有个 `nginx` 的 `pod` 了

```bash
~$ sudo kubectl run nginx --image=nginx
pod/nginx created
~$ sudo kubectl get pod
NAME    READY   STATUS    RESTARTS   AGE
nginx   1/1     Running   0          19s
```

- run: 用于创建并运行一个新的 `pod`
- nginx: `pod` 的名称, 在这个例子中，`pod` 被命名为 `nginx`
- --image=nginx: 指定容器镜像, 这里使用 `Docker Hub` 上的官方 `nginx` 镜像

### 5. create 命令

如创建 `service` 和 `deployment` 等资源, 可以先看帮助获取用法

```bash
sudo kubectl create -h
```

可以看到里面没有 `pod`, 这是因为一般都是使用 `deployment` 等对象自动帮我们创建 `pod`

### 6. 创建 deployment

创建使用 `DockerHub` 上 `nginx` 的最新镜像的 `deployment`, 命名为 `nginx-deployment`

```bash
~$ sudo kubectl create deployment nginx-deployment --image=nginx
deployment.apps/nginx-deployment created
```

- create deployment: 用于创建一个新的 `deployment`
- nginx-deployment: `deployment` 的名称, 这里 `deployment` 被命名为 `nginx-deployment`
- --image=nginx: 指定容器镜像, 这里使用 `Docker Hub` 上的官方 `nginx` 镜像

查看 `deployment`

```bash
~$ sudo kubectl get deployment
NAME               READY   UP-TO-DATE   AVAILABLE   AGE
nginx-deployment   0/1     1            0           19s
```

查看 `replicaset` (副本, 用于管理 `pod` 副本数量), 自动创建并启动

```bash
~$ sudo kubectl get replicaset
NAME                          DESIRED   CURRENT   READY   AGE
nginx-deployment-6d6565499c   1         1         1       3m11s
```

可以看到, `replicaset` 的名字由 `deployment名字-replicaset的ID` 组成, 其中 `replicaset的ID` 由 `pod` 模板的哈希值生成的

查看 `pod`, 可以看到已经自动创建 `pod` 并启动

```bash
~$ sudo kubectl get pod
NAME                                READY   STATUS    RESTARTS   AGE
nginx                               1/1     Running   0          5d22h
nginx-deployment-6d6565499c-dxr2d   1/1     Running   0          48s
```

可以看到, `pod` 的名字由 `deployment名字-replicaset的ID-pod的ID` 组成, 其中 `replicaset的ID` 就是上面查询 `replicaset` 所看到的, 这样就知道这个 `pod` 属于哪个 `replicaset`, 而 `replicaset` 又属于哪个 `deployment` 了

### 7. 创建 service

```bash
sudo kubectl create service nginx-service
```

- create service: 用于创建一个新的 `service`
- nginx-service: `service` 的名称, 这里 `service` 被命名为 `nginx-service`

### 8. 修改 deployment

使用 `edit` 调用系统默认编辑器修改 `deployment` 配置文件, 这里是 `vim`

```bash
sudo kubectl edit deployment nginx-deployment
```

- edit: 进入编辑模式以修改现有资源
- deployment: 指定要编辑的资源类型是 `deployment`
- nginx-deployment: 要编辑的 `deployment` 的名称

找到 `spec.replicas`, 它是用于指定 `pos` 数量的, 这把数量从 1 修改为 3, 然后保存

```vim
spec:               
  progressDeadlineSeconds: 600
  replicas: 3   # 从 1 修改为 3
  revisionHistoryLimit: 10   
```

这时 `K8S` 会自动识别配置文件, 帮我们自动完成操作, 查看 `replicaset`

```bash
~$ sudo kubectl get replicaset
NAME                          DESIRED   CURRENT   READY   AGE
nginx-deployment-6d6565499c   3         3         3       18h
```

查看 `pod`, 已经自动帮我们创建并启动了额外 2 个 `pod`

```bash
~$ sudo kubectl get pod
NAME                                READY   STATUS    RESTARTS   AGE
nginx                               1/1     Running   0          6d16h
nginx-deployment-6d6565499c-dxr2d   1/1     Running   0          18h
nginx-deployment-6d6565499c-nk752   1/1     Running   0          58s
nginx-deployment-6d6565499c-47kk9   1/1     Running   0          58s
```

### 9. 查看日志

先列出我们有哪些 `pod`, 这里列出来后选择 `nginx-deployment` 的最后一个 `pod`, 复制它的名字

```bash
~$ sudo kubectl get pod
NAME                                READY   STATUS    RESTARTS   AGE
nginx                               1/1     Running   0          6d17h
nginx-deployment-6d6565499c-dxr2d   1/1     Running   0          18h
nginx-deployment-6d6565499c-nk752   1/1     Running   0          26m
nginx-deployment-6d6565499c-47kk9   1/1     Running   0          26m
```

使用 `logs` 查看它的日志

```bash
sudo kubectl logs nginx-deployment-6d6565499c-47kk9
```

### 10. 进入容器终端

使用 `exec`, 表示进入某个容器执行命令

```bash
sudo kubectl exec -it nginx-deployment-6d6565499c-47kk9 -- /bin/bash
```

- exec: 这是 `Kubernetes CLI`(命令行接口)的一部分, 用于在容器中运行命令
- -i: 交互式选项, 保持标准输入打开, 以便你可以向容器输入命令
- -t: 分配一个伪终端, 使你可以与容器进行交互
- nginx-deployment-6d6565499c-47kk9: 这是 `Pod` 的名称
- --: 分隔符, 用于将 `kubectl exec` 命令与要在容器中执行的命令分开
- /bin/bash: 这是要在容器中执行的命令, 在这个例子中, 是启动 `Bash shell`

这样就能进入该 `pod` 的终端了

```bash
root@nginx-deployment-6d6565499c-47kk9:/# ls
bin   docker-entrypoint.d   home   media  proc  sbin  tmp
boot  docker-entrypoint.sh  lib    mnt    root  srv   usr
dev   etc                   lib64  opt    run   sys   var
```

### 11. 查看所有资源

使用 `all` 获取所有类型资源

```bash
~$ sudo kubectl get all
NAME                                    READY   STATUS    RESTARTS   AGE
pod/nginx                               1/1     Running   0          6d20h
pod/nginx-deployment-6d6565499c-dxr2d   1/1     Running   0          21h
pod/nginx-deployment-6d6565499c-nk752   1/1     Running   0          3h17m
pod/nginx-deployment-6d6565499c-47kk9   1/1     Running   0          3h17m

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d21h

NAME                               READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/nginx-deployment   3/3     3            3           21h

NAME                                          DESIRED   CURRENT   READY   AGE
replicaset.apps/nginx-deployment-6d6565499c   3         3         3       21h
```

### 12. 删除资源

使用 `delete` 删除资源

```bash
~$ sudo kubectl delete deployment nginx-deployment
deployment.apps "nginx-deployment" deleted
```

- delete: 用于删除资源
- deployment: 指定要删除的资源类型是 `deployment`
- nginx-deployment: 要删除的 `deployment` 的名称

再次查看所有资源

```bash
~$ sudo kubectl get all
NAME        READY   STATUS    RESTARTS   AGE
pod/nginx   1/1     Running   0          6d20h

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d21h
```

## 六、配置文件

### 1. deployment 配置

命名随意, 但规范来说要体现配置的用意和目的, 比如 `nginx-deploy.yaml`, 后缀必须是 `.yaml`

```bash
vi nginx-deploy.yaml
```

#### 1) apiVersion

指定使用的 `api` 版本

```yaml
apiVersion: apps/v1
```

- apps: `group`(组别), 包括 apps(应用), batch(批处理), autoscaling(自动扩缩容) 等
- v1: `version`(版本), 如 v1, v2, v3-beta 等

#### 2) kind

指定资源对象的类型

```yaml
kind: Deployment
```

- 这里是 `Deployment`, 表示用这个配置文件创建 `Deployment` 资源对象
- 其他还有 `Service`, `ConfigMap` 等类型

#### 3) metadata

用来定义资源对象的元数据的, 比如资源对象的 `名字`, `标签`, `命名空间` 等

```yaml
metadata:
  name: nginx-deploy
```

#### 4) spec

`specification` 的缩写, 即 `规范`, 用来定义各种资源对象的配置信息, 包括有多少副本, 使用什么镜像, 暴露哪个端口等

```yaml
spec:
  selector:
    matchLabels:
      app: nginx
  replicas: 3
  template:
    metadata:
      labels:
        app: nginx
    spec:
      containers:
        - name: nginx
          image: nginx:1.25
          ports:
            - containerPort: 80
```

- 可以看到有两个 `spec` 嵌套
- 外层的 `spec` 用来定义 `deployment` 的配置信息
- 里层的 `spec` 用来定义 `pod` 的配置信息
- `replicas` 的值是 3, 表示有 3 个副本
- `template` 定了的 `pod` 的配置信息, 如使用镜像是 `nginx 1.25` 版本, 暴露的端口是 `80`

#### 5) 使用配置文件创建资源对象

依然使用 `create` 创建 `deployment`, 但要加上 `-f` 指定该配置文件

```bash
~$ sudo kubectl create -f ~/nginx-deploy.yaml
deployment.apps/nginx-deploy created
```

然后获取所有资源可以看到

```bash
~$ sudo kubectl get all
NAME                               READY   STATUS    RESTARTS   AGE
pod/nginx                          1/1     Running   0          6d21h
pod/nginx-deploy-cd5968d5b-89747   1/1     Running   0          2m34s
pod/nginx-deploy-cd5968d5b-hbzvp   1/1     Running   0          2m34s
pod/nginx-deploy-cd5968d5b-pd88x   1/1     Running   0          2m34s

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d22h

NAME                           READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/nginx-deploy   3/3     3            3           2m34s

NAME                                     DESIRED   CURRENT   READY   AGE
replicaset.apps/nginx-deploy-cd5968d5b   3         3         3       2m34s
```

#### 6) 使用配置文件删除资源对象

只需把 `create` 替换为 `delete` 即可

```bash
~$ sudo kubectl delete -f ~/nginx-deploy.yaml
deployment.apps "nginx-deploy" deleted
```

再次查询所有资源

```bash
~$ sudo kubectl get all
NAME        READY   STATUS    RESTARTS   AGE
pod/nginx   1/1     Running   0          6d21h

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d22h
```

#### 7) 应用配置文件

将 `create` 替换为 `apply`, 可以创建或更新配置资源, 如果资源对象不存在则创建, 如果存在则根据配置文件的内容更新资源对象

```bash
~$ sudo kubectl apply -f ~/nginx-deploy.yaml
deployment.apps/nginx-deploy created
```

然后获取所有资源

```bash
~$ sudo kubectl get all
NAME                               READY   STATUS    RESTARTS   AGE
pod/nginx                          1/1     Running   0          6d21h
pod/nginx-deploy-cd5968d5b-mpshp   1/1     Running   0          53s
pod/nginx-deploy-cd5968d5b-5n6wp   1/1     Running   0          53s
pod/nginx-deploy-cd5968d5b-xwgbb   1/1     Running   0          53s

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d22h

NAME                           READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/nginx-deploy   3/3     3            3           53s

NAME                                     DESIRED   CURRENT   READY   AGE
replicaset.apps/nginx-deploy-cd5968d5b   3         3         3       53s
```

可以看到找不到资源对象会自动创建, 然后再修改配置文件

```bash
vi nginx-deploy.yaml
```

把 `replicas` 改为 5

```yaml
spec:
  ...
  replicas: 5
  ...
```

再次应用配置文件

```bash
~$ sudo kubectl apply -f ~/nginx-deploy.yaml
deployment.apps/nginx-deploy configured
```

然后查看所有资源, 可以看到 `pod` 变成了 5 个

```bash
~$ sudo kubectl get all
NAME                               READY   STATUS    RESTARTS   AGE
pod/nginx                          1/1     Running   0          6d21h
pod/nginx-deploy-cd5968d5b-mpshp   1/1     Running   0          6m55s
pod/nginx-deploy-cd5968d5b-5n6wp   1/1     Running   0          6m55s
pod/nginx-deploy-cd5968d5b-xwgbb   1/1     Running   0          6m55s
pod/nginx-deploy-cd5968d5b-crfq8   1/1     Running   0          50s
pod/nginx-deploy-cd5968d5b-l4w7f   1/1     Running   0          50s

NAME                 TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
service/kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   6d22h

NAME                           READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/nginx-deploy   5/5     5            5           6m55s

NAME                                     DESIRED   CURRENT   READY   AGE
replicaset.apps/nginx-deploy-cd5968d5b   5         5         5       6m55s
```

### 2. service 配置

命名随意, 但规范来说要体现配置的用意和目的, 比如 `nginx-service.yaml`, 后缀必须是 `.yaml`

```bash
vi nginx-service.yaml
```

#### 1) apiVersion

指定使用的 `api` 版本

```yaml
apiVersion: v1
```

#### 2) kind

指定资源对象的类型

```yaml
kind: Service
```

#### 3) metadata

用来定义资源对象的元数据

```yaml
metadata:
  name: nginx-service
```

#### 4) spec

`specification` 的缩写, 即 `规范`, 用来定义服务的规格和描述部分

```yaml
spec:
  selector:
    app: nginx
  ports:
    - protocol: TCP
      port: 80
      targetPort: 80
```

- selector: 选择器, 用来选择特定资源的配置, 这里配置的是所有 `app` 是 `nginx` 的特定资源, 一般和 `label` 标签一起使用, 会根据该配置去查找 `label` 为 `app=nginx` 的 `pod`, 很多时候我们都是使用 `selector` 和 `label` 来对集群资源进行分类和选择
- protocol: 协议类型, 这里配置 `TCP`
- port: 该服务对外的端口
- targetPort: 该房屋与 `pod` 连接时, `pod` 对应的端口

#### 5) 应用配置文件

依然使用 `apply`, 可以创建或更新配置资源, 如果资源对象不存在则创建, 如果存在则根据配置文件的内容更新资源对象

```bash
~$ sudo kubectl apply -f ~/nginx-service.yaml
service/nginx-service created
```

查看服务, 可以看到服务 `nginx-service` 已经被创建并启动

```bash
~$ sudo kubectl get service
NAME            TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)   AGE
kubernetes      ClusterIP   10.43.0.1      <none>        443/TCP   7d
nginx-service   ClusterIP   10.43.135.69   <none>        80/TCP    5s
```

使用 `curl` 命令可以校验是否正常访问服务

```bash
~$ curl 10.43.135.69
<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
<style>
html { color-scheme: light dark; }
body { width: 35em; margin: 0 auto;
font-family: Tahoma, Verdana, Arial, sans-serif; }
</style>
</head>
<body>
<h1>Welcome to nginx!</h1>
<p>If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.</p>

<p>For online documentation and support please refer to
<a href="http://nginx.org/">nginx.org</a>.<br/>
Commercial support is available at
<a href="http://nginx.com/">nginx.com</a>.</p>

<p><em>Thank you for using nginx.</em></p>
</body>
</html>
```

## 七、配置和公开服务

在查看 `pod` 的基础上, 加上 `-o wide` 可以查看 `pod` 在那个 `worker` 和 `ip` 地址

```bash
~$ sudo kubectl get pod
NAME                           READY   STATUS    RESTARTS   AGE
nginx                          1/1     Running   0          6d22h
nginx-deploy-cd5968d5b-mpshp   1/1     Running   0          65m
nginx-deploy-cd5968d5b-5n6wp   1/1     Running   0          65m
nginx-deploy-cd5968d5b-xwgbb   1/1     Running   0          65m
nginx-deploy-cd5968d5b-crfq8   1/1     Running   0          59m
nginx-deploy-cd5968d5b-l4w7f   1/1     Running   0          59m
~$ 
~$ 
~$ sudo kubectl get pod -o wide
NAME                           READY   STATUS    RESTARTS   AGE     IP          NODE      NOMINATED NODE   READINESS GATES
nginx                          1/1     Running   0          6d22h   10.42.1.3   worker1   <none>           <none>
nginx-deploy-cd5968d5b-mpshp   1/1     Running   0          65m     10.42.2.6   worker2   <none>           <none>
nginx-deploy-cd5968d5b-5n6wp   1/1     Running   0          65m     10.42.3.5   worker3   <none>           <none>
nginx-deploy-cd5968d5b-xwgbb   1/1     Running   0          65m     10.42.1.6   worker1   <none>           <none>
nginx-deploy-cd5968d5b-crfq8   1/1     Running   0          59m     10.42.2.7   worker2   <none>           <none>
nginx-deploy-cd5968d5b-l4w7f   1/1     Running   0          59m     10.42.0.9   k3s       <none>           <none>
```

而这些 `pod` 对于的 `ip` 只能在集群内部访问, 且 `pod` 一旦销毁重启, `ip` 地址会改变, 所以就需要用到 `service` 组件了

### 1. 公开服务

可以使用 `expose` 一个 `deployment` 来公开一个服务

```bash
~$ sudo kubectl expose deployment nginx-deploy
service/nginx-deploy exposed
```

查看服务, 可以看到多出了一个 `nginx-deploy` 的服务, 且 `PORT` 端口为 `nginx-deploy.yaml` 里配置的 `80`

```bash
~$ sudo kubectl get service
NAME           TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)   AGE
kubernetes     ClusterIP   10.43.0.1      <none>        443/TCP   7d
nginx-deploy   ClusterIP   10.43.98.204   <none>        80/TCP    68s
```

使用 `curl` 命令可以校验是否正常访问服务

```bash
~$ curl 10.43.98.204
<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
<style>
html { color-scheme: light dark; }
body { width: 35em; margin: 0 auto;
font-family: Tahoma, Verdana, Arial, sans-serif; }
</style>
</head>
<body>
<h1>Welcome to nginx!</h1>
<p>If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.</p>

<p>For online documentation and support please refer to
<a href="http://nginx.org/">nginx.org</a>.<br/>
Commercial support is available at
<a href="http://nginx.com/">nginx.com</a>.</p>

<p><em>Thank you for using nginx.</em></p>
</body>
</html>
```

### 2. 查看服务描述

使用 `describe` 查看服务详情

```bash
~$ sudo kubectl describe service nginx-deploy
Name:              nginx-deploy
Namespace:         default
Labels:            <none>
Annotations:       <none>
Selector:          app=nginx
Type:              ClusterIP
IP Family Policy:  SingleStack
IP Families:       IPv4
IP:                10.43.98.204
IPs:               10.43.98.204
Port:              <unset>  80/TCP
TargetPort:        80/TCP
Endpoints:         10.42.0.9:80,10.42.1.6:80,10.42.2.6:80 + 2 more...
Session Affinity:  None
Events:            <none>
```

- describe: 用于显示资源的详细信息
- service: 指定要查看的资源类型为 `Service`
- nginx-deploy: `Service` 的名称, 在这个例子中, 是 `nginx-deploy`
- 也可查看其他资源, 如 `pod`, `deployment` 等

### 3. 删除服务

使用 `delete` 命令删除

```bash
~$ sudo kubectl get service
NAME           TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)   AGE
kubernetes     ClusterIP   10.43.0.1      <none>        443/TCP   7d
nginx-deploy   ClusterIP   10.43.98.204   <none>        80/TCP    19m
~$ 
~$ sudo kubectl delete service nginx-deploy
service "nginx-deploy" deleted
~$ 
~$ sudo kubectl get service
NAME         TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)   AGE
kubernetes   ClusterIP   10.43.0.1    <none>        443/TCP   7d
```

- delete: 删除一个资源
- service: 指定要删除的资源类型为 `service`
- nginx-deploy: 指定要删除的资源名字, 这里是 `nginx-deploy`

### 4. NodePort

目前集群外部依然访问不了服务, 这时就需要使用 `NodePort` 将服务 `ip` 和 端口 映射到外部以供访问

这里采用 [service文件配置](#service-配置) 方式来启动服务, 并修改配置

```bash
vi nginx-service.yaml
```

在 `spec` 下添加 `type`, 并加入值为 `NodePort`, 表示节点端口类型服务

```yaml
spec:
  type: NodePort
```

- type: 指定服务的类型, 不配置时默认为 `ClusterIP`
- ClusterIP: 默认类型, 集群内部服务
- NodePort: 节点端口类型, 将服务公开到集群节点上
- LoadBalancer: 负载均衡类型, 将服务公开到外部负载均衡器上
- ExternalName: 外部名称类型, 将服务映射到一个外部域名上
- Headless: 无头类型, 主要用于 `DNS` 解析和服务发现, 但此时 `service` 不再提供负载均衡功能

在 `spec.ports` 添加 `nodePort` 指定端口, 这里为 `30080`(范围必须 `30000` ~ `32767` 之间)

```yaml
spec:
  ...
  ports:
    - protocol: TCP
      port: 80
      targetPort: 80
      nodePort: 30080
```

然后应用服务

```bash
~$ sudo kubectl apply -f ~/nginx-service.yaml
service/nginx-service configured
```

查看服务, 可以看到 `nginx-service` 的 `TYPE` 已经是 `NodePort` 了

```bash
~$ sudo kubectl get service -o wide
NAME            TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)        AGE    SELECTOR
kubernetes      ClusterIP   10.43.0.1      <none>        443/TCP        7d1h   <none>
nginx-service   NodePort    10.43.135.69   <none>        80:30080/TCP   28m    app=nginx
```

这时我们要从外部服务集群, 需要知道节点的 `ip` 地址

```bash
~$ sudo kubectl get node -o wide
NAME      STATUS   ROLES                  AGE    VERSION        INTERNAL-IP    EXTERNAL-IP   OS-IMAGE           KERNEL-VERSION     CONTAINER-RUNTIME
worker2   Ready    <none>                 7d     v1.29.5+k3s1   192.168.64.6   <none>        Ubuntu 24.04 LTS   6.8.0-31-generic   containerd://1.7.15-k3s1
k3s       Ready    control-plane,master   7d1h   v1.29.5+k3s1   192.168.64.4   <none>        Ubuntu 24.04 LTS   6.8.0-31-generic   containerd://1.7.15-k3s1
worker1   Ready    <none>                 7d     v1.29.5+k3s1   192.168.64.5   <none>        Ubuntu 24.04 LTS   6.8.0-31-generic   containerd://1.7.15-k3s1
worker3   Ready    <none>                 25h    v1.29.5+k3s1   192.168.64.7   <none>        Ubuntu 24.04 LTS   6.8.0-31-generic   containerd://1.7.15-k3s1
```

复制 `master` 节点的 `ip` 地址 `192.168.64.4`, 到浏览器输入 `192.168.64.4:30080` 即可服务 `nginx` 服务了
