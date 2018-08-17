# Websocket with client-go example

At the moment [kubernetes/client-go](https://github.com/kubernetes/client-go) does not natively support websocket connections. However the SDK provides everything we need to wrap our own websocket connection handler with the necessary Kubernetes security context, to allow us to authenticate to the apiserver.

This demo shows, how to write your own `kubectl exec` in go with the Kubernetes SDK.


## Building

```bash
go get github.com/rmohr/kubernetes-custom-exec
cd $GOPATH/src/github.com/rmohr/kubernetes-custom-exec
go build
```

# Running

To execute the command `ls` on pod `virt-api`, in container `virt-api`, type

```bash
./kubernetes-custom-exec -kubeconfig ~/kubeconfig -pod virt-api -namespace default -command ls -container virt-api
anaconda-post.log
bin
dev
etc
home
lib
lib64
lost+found
media
mnt
opt
proc
root
run
sbin
srv
sys
third_party
tmp
usr
var
virt-api
```
