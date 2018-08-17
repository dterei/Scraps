package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	v1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/kubernetes/scheme"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"
	"k8s.io/client-go/tools/remotecommand"
	"k8s.io/client-go/util/exec"
)

// Args for exec.
type Args struct {
	Pod       string
	Container string
	Namespace string
	Command   []string
	Arg       string
}

type writer struct {
	Str []string
}

func (w *writer) Write(p []byte) (n int, err error) {
	str := string(p)
	if len(str) > 0 {
		w.Str = append(w.Str, str)
	}
	return len(str), nil
}

func homeDir() string {
	if h := os.Getenv("HOME"); h != "" {
		return h
	}
	return os.Getenv("USERPROFILE")
}

func getK8SConfigInternal() *rest.Config {
	config, err := rest.InClusterConfig()
	if err != nil {
		panic(err.Error())
	}
	return config
}

func getK8SConfigExternal() *rest.Config {
	var kubeconfig *string
	if home := homeDir(); home != "" {
		kubeconfig = flag.String(
			"kubeconfig",
			filepath.Join(home, ".kube", "config"),
			"(optional) absolute path to the kubeconfig file",
		)
	} else {
		kubeconfig = flag.String(
			"kubeconfig",
			"",
			"absolute path to the kubeconfig file",
		)
	}
	flag.Parse()

	// use the current context in kubeconfig
	config, err := clientcmd.BuildConfigFromFlags("", *kubeconfig)
	if err != nil {
		panic(err.Error())
	}

	return config
}

func newK8SClient(config *rest.Config) *kubernetes.Clientset {
	kubeClient, err := kubernetes.NewForConfig(config)
	if err != nil {
		panic(err.Error())
	}
	return kubeClient
}

func kubeExec(
	config *rest.Config,
	k8s *kubernetes.Clientset,
	args *Args,
) error {
	// check pod exists
	pod, err :=
		k8s.CoreV1().Pods(args.Namespace).Get(args.Pod, metav1.GetOptions{})
	if err != nil {
		return err
	}

	// check container exists
	if args.Container != "" {
		notFound := true
		for _, container := range pod.Spec.Containers {
			if container.Name == args.Container {
				notFound = false
				break
			}
		}
		if notFound {
			return fmt.Errorf(`Container "%v" not found`, args.Container)
		}
	}

	// execute command
	req := k8s.CoreV1().RESTClient().Post().
		Resource("pods").
		Name(args.Pod).
		Namespace(args.Namespace).
		SubResource("exec").
		Param("container", args.Container)
	req.VersionedParams(&v1.PodExecOptions{
		Container: args.Container,
		Command:   args.Command,
		Stdin:     true,
		Stdout:    true,
		Stderr:    true,
	}, scheme.ParameterCodec)

	rc, err := remotecommand.NewSPDYExecutor(config, "POST", req.URL())
	if err != nil {
		return err
	}

	stdIn := strings.NewReader(args.Arg)
	stdOut := &writer{}
	stdErr := &writer{}

	err = rc.Stream(remotecommand.StreamOptions{
		Stdin:  stdIn,
		Stdout: stdOut,
		Stderr: stdErr,
		Tty:    false,
	})

	var exitCode int
	if err == nil {
		exitCode = 0
	} else {
		if exitErr, ok := err.(exec.ExitError); ok && exitErr.Exited() {
			exitCode = exitErr.ExitStatus()
		} else {
			return fmt.Errorf("failed to find exit code")
		}
	}

	fmt.Printf("Exit Code: %d\n", exitCode)
	fmt.Printf("Stdout: %s\n", strings.Join(stdOut.Str, "|"))
	fmt.Printf("Stderr: %s\n", strings.Join(stdErr.Str, "|"))

	return nil
}

func main() {
	args := &Args{
		Pod:       "exocompute-containers-example-loop",
		Container: "exocompute-containers-example-loop",
		Namespace: "default",
		Command:   []string{"echo", "hello"},
		Arg:       "1",
	}

	config := getK8SConfigInternal()
	k8s := newK8SClient(config)
	err := kubeExec(config, k8s, args)
	if err != nil {
		fmt.Printf("Error executing command: %v\n", err)
	}
}
