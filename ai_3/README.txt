Hi Jim!
Hope everything is going well.
In this submission the file structure is a bit changed, so I want to write a note to summarise it.

You need the following core files without any changes: "grid-class.ss" / "grid-draw.ss" / "grid-make.ss" / "grid-new.ss"
"grid-RTA.ss" --> This file has the Real-Time A* Search
"grid-HC.ss" --> This file has the Hill Climber Search
"grid-main.ss" --> I modified the old main to use the same grid on two different search functions
"grid-boss.ss" --> Boss of everything:). It calls grid-main and at the end returns the average result for each

So please run "grid-boss.ss" rather than "grid-main.ss". Otherwise, you'll only see 1 test case and no average.

I wanted to clarify it. Best,

Derin