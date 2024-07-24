#!/bin/bash

set -ex

registry_host=192.168.64.2:5000
app_list=(erlang-docker-a erlang-docker-b)

for app in ${app_list[@]}; do

    kubectl delete statefulset ${app}-sfs
    kubectl delete service ${app}-svc
    kubectl delete service ${app}-hl

    docker system prune -a -f
    docker exec registry rm -rf /var/lib/registry/docker/registry/v2/repositories/${app}
    docker exec registry bin/registry garbage-collect /etc/docker/registry/config.yml
    docker restart registry

    docker load -i ${app}.tar
    docker tag ${app} ${registry_host}/${app}
    docker push ${registry_host}/${app}

    kubectl apply -f k8s_yaml/erlang-docker-storageclass.yaml
    kubectl apply -f k8s_yaml/${app}/${app}-service.yaml
    kubectl apply -f k8s_yaml/${app}/${app}-headless.yaml
    kubectl apply -f k8s_yaml/${app}/${app}-statefulset.yaml

done

