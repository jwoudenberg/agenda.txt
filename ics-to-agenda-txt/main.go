package main

import (
	"bufio"
	"fmt"
	"github.com/apognu/gocal"
	"os"
	"time"
)

func main() {
	var start time.Time

	if len(os.Args) > 1 {
		var err error
		start, err = time.Parse("2006-01-02", os.Args[1])
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			os.Exit(1)
		}
	} else {
		start = time.Now()
	}

	stdin := bufio.NewReader(os.Stdin)

	end := start.Add(12 * 30 * 24 * time.Hour)

	c := gocal.NewParser(stdin)
	c.Start, c.End = &start, &end
	if err := c.Parse(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	for _, e := range c.Events {
		formattedStart := e.Start.In(time.Local).Format("2006-01-02 15:04")
		formattedDuration := fmt.Sprintf("%d:%02d", int(e.Duration.Hours()), int(e.Duration.Minutes())%60)
		fmt.Printf("%s +%s %s (%s)\n", formattedStart, formattedDuration, e.Summary, e.Location)
	}
}
