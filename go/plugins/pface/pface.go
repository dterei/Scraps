// Package pface defines an interface all plugins should implement.
package pface

// Plugin is the expected interface (and type) of all plugins.
type Plugin interface {
	Initialize(Config) error
	ID() string
	Run() error
}

// Config is the configuration of the main application and is passed to all
// plugins so they can configure themselves.
type Config interface {
	AppVersion() string
}
