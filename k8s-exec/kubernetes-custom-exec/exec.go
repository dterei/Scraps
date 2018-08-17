package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"

	"github.com/golang/glog"
	"github.com/gorilla/websocket"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"
)

func main() {

	var kubeconfig string
	var master string
	var pod string
	var namespace string
	var container string
	var command string

	flag.StringVar(&kubeconfig, "kubeconfig", "", "absolute path to the kubeconfig file")
	flag.StringVar(&master, "master", "", "master url")
	flag.StringVar(&pod, "pod", "", "pod")
	flag.StringVar(&namespace, "namespace", "", "namespace")
	flag.StringVar(&container, "container", "", "container")
	flag.StringVar(&command, "command", "", "command")
	flag.Parse()

	// creates the connection
	config, err := clientcmd.BuildConfigFromFlags(master, kubeconfig)
	if err != nil {
		glog.Fatal(err)
	}

	// Create a round tripper with all necessary kubernetes security details
	wrappedRoundTripper, err := roundTripperFromConfig(config)
	if err != nil {
		log.Fatalln(err)
	}

	// Create a request out of config and the query parameters
	req, err := requestFromConfig(config, pod, container, namespace, command)
	if err != nil {
		log.Fatalln(err)
	}

	// Send the request and let the callback do its work
	_, err = wrappedRoundTripper.RoundTrip(req)

	if err != nil {
		log.Fatalln(err)
	}
}

type RoundTripCallback func(conn *websocket.Conn, resp *http.Response, err error) error

type WebsocketRoundTripper struct {
	Dialer *websocket.Dialer
	Do     RoundTripCallback
}

func (d *WebsocketRoundTripper) RoundTrip(r *http.Request) (*http.Response, error) {
	conn, resp, err := d.Dialer.Dial(r.URL.String(), r.Header)
	if err == nil {
		defer conn.Close()
	}
	return resp, d.Do(conn, resp, err)
}

func WebsocketCallback(ws *websocket.Conn, resp *http.Response, err error) error {

	if err != nil {
		if resp != nil && resp.StatusCode != http.StatusOK {
			buf := new(bytes.Buffer)
			buf.ReadFrom(resp.Body)
			return fmt.Errorf("Can't connect to console (%d): %s\n", resp.StatusCode, buf.String())
		}
		return fmt.Errorf("Can't connect to console: %s\n", err.Error())
	}

	txt := ""
	for {
		_, body, err := ws.ReadMessage()
		if err != nil {
			fmt.Println(txt)
			if err == io.EOF {
				return nil
			}
			return err
		}
		txt = txt + string(body)
	}
}

func roundTripperFromConfig(config *rest.Config) (http.RoundTripper, error) {

	// Configure TLS
	tlsConfig, err := rest.TLSConfigFor(config)
	if err != nil {
		return nil, err
	}

	// Configure the websocket dialer
	dialer := &websocket.Dialer{
		Proxy:           http.ProxyFromEnvironment,
		TLSClientConfig: tlsConfig,
	}

	// Create a roundtripper which will pass in the final underlying websocket connection to a callback
	rt := &WebsocketRoundTripper{
		Do:     WebsocketCallback,
		Dialer: dialer,
	}

	// Make sure we inherit all relevant security headers
	return rest.HTTPWrappersForConfig(config, rt)
}

func requestFromConfig(config *rest.Config, pod string, container string, namespace string, cmd string) (*http.Request, error) {

	u, err := url.Parse(config.Host)
	if err != nil {
		return nil, err
	}

	switch u.Scheme {
	case "https":
		u.Scheme = "wss"
	case "http":
		u.Scheme = "ws"
	default:
		return nil, fmt.Errorf("Malformed URL %s", u.String())
	}

	u.Path = fmt.Sprintf("/api/v1/namespaces/%s/pods/%s/exec", namespace, pod)
	if container != "" {
		u.RawQuery = "command=" + cmd +
			"&container=" + container +
			"&stderr=true&stdout=true" +
			"&stdout=true" +
			"&stederr=true"
	}
	req := &http.Request{
		Method: http.MethodGet,
		URL:    u,
	}

	return req, nil
}
