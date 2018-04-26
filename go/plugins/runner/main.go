package main

import (
	"flag"
	"fmt"
	"log"
	"plugin"

	"../pface"
)

func init() {
	log.Printf("Runner init...\n")
}

type config struct {
	plugin1 string
	plugin2 string
}

func parseArgs() *config {
	p1 := flag.String("plugin1", "", "Plugin to load and run")
	p2 := flag.String("plugin2", "", "Plugin to load and run")

	flag.Parse()

	return &config{
		plugin1: *p1,
		plugin2: *p2,
	}
}

func main() {
	conf := parseArgs()

	// load ad-hoc p1 plugin
	fmt.Printf("\n")
	log.Printf("=== p1 ===\n")

	plug1, err := plugin.Open(conf.plugin1)
	if err != nil {
		log.Fatalf("Error opening plugin at path '%s': %v", conf.plugin1, err)
	}

	vSym, err := plug1.Lookup("V")
	if err != nil {
		log.Fatalf("Couldn't find symbol 'V': %v", err)
	}

	fSym, err := plug1.Lookup("F")
	if err != nil {
		log.Fatalf("Couldn't find symbol 'F': %v", err)
	}

	v, ok := vSym.(*int)
	if !ok {
		log.Fatalf("'V' is of wrong type: %T", vSym)
	}

	f, ok := fSym.(func())
	if !ok {
		log.Fatalf("'F' is of wrong type: %T", fSym)
	}

	*v = 7
	f()

	// load typed plugin p2
	fmt.Printf("\n")
	log.Printf("=== p2 ===\n")

	plug2, err := plugin.Open(conf.plugin2)
	if err != nil {
		log.Fatalf("Error opening plugin at path '%s': %v", conf.plugin2, err)
	}

	p2Sym, err := plug2.Lookup("Plugin")
	if err != nil {
		log.Fatalf("Couldn't find symbol 'Plugin': %v", err)
	}

	p2, ok := p2Sym.(pface.Plugin)
	if !ok {
		log.Fatalf("'Plugin' is of wrong type: %T", p2Sym)
	}

	if err = p2.Initialize(&pluginsConf{"RunnerV1"}); err != nil {
		log.Fatalf("Couldn't initialize plugin 'p2': %v", err)
	}
	if err = p2.Run(); err != nil {
		log.Fatalf("Couldn't run plugin 'p2': %v", err)
	}

	log.Printf("=== End: %s ===\n", p2.ID())
}

type pluginsConf struct {
	appVersion string
}

func (p *pluginsConf) AppVersion() string {
	return p.appVersion
}
