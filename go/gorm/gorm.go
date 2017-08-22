package main

import (
	"fmt"
	"os"
	"database/sql"

	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/mysql" // required for sql
)

const (
	dsn = "root:spark@/rubrik_dev"
)

func debug() error {
	dbSql, err := sql.Open("mysql", dsn)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return err
	}
	err = dbSql.Ping()
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return err
	}

	db, err := gorm.Open(
		"mysql",
		dbSql)
	// 	fmt.Sprintf("%s?parseTime=true", dsn))
	if err != nil {
		return err
	}
	defer db.Close()
	db.LogMode(true)

	// Query: Raw, Results: Struct
	type T1 struct {
		ID   int
		Name string
		Age  int
	}
	var t1 T1
	if err := db.Where("id = ?", 1).First(&t1).Error; err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("%v\n", t1)
	}

	// Query: Struct, Results: Struct
	var t2 T1
	if err := db.Where(&T1{ID: 2}).First(&t2).Error; err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("%v\n", t2)
	}

	// Query: Raw, Results: Raw
	var name string
	if err := db.Table("t1").Where("id = ?", 3).Select("name").Row().Scan(&name); err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("%v\n", name)
	}

	// Query: Raw, Results: Raw
	var id int
	if err := db.Table("t1").Select("MAX(id)").Row().Scan(&id); err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("%v\n", id)
	}

	fmt.Println("done...")
	os.Exit(0)
	return nil
}

func main() {
	debug()
}
