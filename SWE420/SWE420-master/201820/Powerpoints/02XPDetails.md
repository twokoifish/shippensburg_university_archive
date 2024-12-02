XP Details - Practices & Context
=====

# Environment Practices
 * Sit together
 * big space owned by team
 * pair programming zone
 * planning area (stand up table)
 * comfy reading/thought space (quiet)
 * group design zone
 * touch down zone
 * Informative Workspace
 * Stories/Tasks on the wall
   * categorized by state
   * as a chart (%complete vs. quality)
 * Information Radiators
   * burn down chart
   * lava lamp
   * defect rate over time
   * whatever you are trying to work on
   * remove things that are no longer changing
   * Info is for us and for anyone who is interested
   * shows we are not hiding anything (builds trust)
 * Whole Team
   * All of the disciplines work together (developers, designers, graphic designers, DBAs, testers, customer, etc.)
   * Need to feel “belonging” - we are in this together
   * We succeed or fail together
   * 12 & 150 discontinuities:
     * 12 is the number of people we can comfortably interact with in a day
     * If team is bigger than 150, we no longer recognize everyone’s faces
     * Crossing those boundaries creates a lack of trust

# Work Practices
 * Energized Work (sustainable pace)
   * Why do we work longer?
     * It is gives us some sort of control
     * Why is it bad?
     * “With enough caffeine and sugar, I can keep typing long past the point where I have started removing value from the project.” - and you won’t know you are doing it
 * Pair Programming
   * One machine, keyboard & mouse
   * roles
     * pilot 
       * has keyboard
       * is generating code
       * narrates where he is going
     * co-pilot
       * thinking in the bigger picture
       * researching details
     * Change roles at least every 5 minutes (testing game)
   * keep each other on task
   * clarify ideas
   * help each other get unstuck
   * make sure we remember our other practices

Can you ever code alone?  ONLY prototypes - code it again together

Change partners frequently (based on a timer?)

Compare pair programming to formal code review

# Planning Practices
## Stories
 * Customer-visible functionality
 * A sentence or two on an index card
 * Estimate of development time
 * creates a conversation between business and technical perspectives
 * educates business to help them make better choices
 * Split them if they are too big
 * Combine them when it makes sense
 * Refine them
 * Give it a short name
 * Put them where everyone can see them

## Iteration Planning
 * fixed length, timeboxed iterations (3 weeks in 1st edition, 1 week in 2nd)
 * Start with a meeting with a customer to pick the stories
 * Break stories into tasks
 * Team members sign up for tasks and estimate them

## Quarterly Cycle
 * Once a quarter reflect on team, project, progress, and goals
 * Identify bottlenecks
 * Plan themes for the quarter

## Slack
He has a few definitions
 * “minor tasks that can be dropped if you get behind”
 * underestimate (use 2 wheel drive until you are stuck)
 * plan 20 % “geek week” 

# Technical Practices
## Ten-Minute Build
 * Automatic build and full run of tests should take no longer than 10 minutes
 * So we run them often and have time for coffee

## Continuous Integration
 * Integrate and test changes after no more than a couple of hours
 * the longer you are out on your own, the harder integration will be
 * asynchronous
   * build system runs it every now and then
 * synchronous
   * it happens when we commit (best - we haven’t moved on, yet)

# Technical Practices
## Test-Driven Development: Red, Green, Refactor
 * Reduces scope creep
 * Make looser coupling and more cohesion
 * Builds trust with your teammates
 * Rhythm and direction

## Incremental Design
 * We aren’t just coding - we have to keep in mind the design of the system
 * Cost of change over time
 * dogma says it increases as the project progresses
 * that’ll kill us
 * hold it down by keeping the code clean
 * Defer design choices to the last responsible moment
 * Design debt

## Negotiated Scope
 * Planning Game helps the customer weigh risks and benefits of developing each story

# Corollary Practices
 * Real Customer Involvement
 * Incremental Deployment
 * Team Continuity
 * Shrinking Team
   * as they get better, reduce the team’s size (??)
 * Root Cause Analysis
   * Every time we see a defect, we figure out what caused it and fix that, too
   * For technical issues
     * write a test that demonstrates the problem as reported
     * write a unit test of the smallest scope possible that reproduces the problem
     * Fix them problem (both tests should then pass)
     * Look at what caused the defect
     * Ask five times why a problem occurred (like a two year old)
 * Shared Code
   * Anyone can fix any code at any time
   * If no one person is responsible for a section of code, everyone will act irresponsibly
     * really?
   * We all have areas of expertise and might want to ask a question before we change someone else’s code
   * It isn’t that no one owns anything - it is that everyone is responsible for everything

# How does this get contracted?
 * Sequence of short contracts
 * Negotiated Scope Contract
 * Think about pay-per-use
   * Customers only pay for what they use 
   * Development is motivated to build features that will be useful

