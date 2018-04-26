package main

import (
	"fmt"

	"../pface"
)

var Plugin MyPlugin

func init() {
	// can use init function to do some setup before plugin is executed
	Plugin.x = 1
}

type MyPlugin struct {
	conf pface.Config
	x    int
}

func (p *MyPlugin) ID() string {
	return "p2"
}

func (p *MyPlugin) Initialize(conf pface.Config) error {
	p.conf = conf
	return nil
}

func (p *MyPlugin) Run() error {
	fmt.Printf("X  : %d\n", p.x)
	fmt.Printf("App: %s\n", p.conf.AppVersion())
	return nil
}
