---
title: Functional Programming courses
---

Courses to kickstart or further your knowledge of functional programming. We have introductory courses for people starting out, and an applied course for those that are ready for more.

All of our course material is open source and we encourage everyone to provide feedback through either issues or pull-requests. These courses are for you and the community, so if we can make them better then we want to know!

### Current Courses

---

#### Introduction to Functional Programming

Course Repository: [https://github.com/data61/fp-course](https://github.com/data61/fp-course)

This is the primary course that we run, the course aims to introduce functional programming to people who already have some exposure to programming.

This course requires no prior experience with Functional Programming. However, general familiarity with the programming workflow (text editor, command line use, revision control) is important.

Many people from around the world have worked through it on their own and found it to be valuable. The course is self-contained but has been designed with personal guidance in mind &mdash; we recommend coming along to experience it that way if you have the chance.

#### Applied Functional Programming

Course Repository: [https://github.com/qfpl/applied-fp-course](https://github.com/qfpl/applied-fp-course)

This is new follow-up course that is targeted toward programmers who have become proficient with navigating functional programming concepts and would like to progress to building a complete application. This course material involves writing a REST application with a database backend.

Topics covered include:

- Hackage and the general Haskell ecosystem
- Cabal files and project structure
- Database integration
- Configuration
- Monad transformers
- Testing
- Type driven development

The assumed Haskell knowledge for this course is that you are comfortable with ``Applicative``, and you are able to write the definition for a function with the type: ``Applicative f => [f a] -> f [a]``

### Outcomes & Requirements

---

#### Key Outcomes

* you should be able to accurately describe the benefits of Functional Programming as they apply in practice.
* you should gain the confidence to continue exploring your own self-directed learning of Functional Programming.
* you should feel confident to seek out the necessary skills that are required to apply Functional Programming in your professional programming role.
* you will be shown the location of online (volunteer) peer support networks, for individuals who choose to continue self-study and require assistance from others to do so.

#### Requirements for participation

Participants will be required to bring a suitable development machine (e.g. portable laptop) for working through the exercises. You will also need to install the Glasgow Haskell Compiler ([http://www.haskell.org/ghc/](http://www.haskell.org/ghc/)) version 7.10 or higher on that machine.

You will require a copy of the course repository on your development machine:

##### For the Introductory Course:

* [https://github.com/data61/fp-course/](https://github.com/data61/fp-course/)
* There are no other primary requirements. However, reading up on Haskell syntax prior to the introductory material will be beneficial to progress.

##### For the Applied Course:

* [https://github.com/qfpl/applied-fp-course/](https://github.com/qfpl/applied-fp-course/).
* Working install of SQLite ([https://www.sqlite.org/quickstart.html](https://www.sqlite.org/quickstart.html)).

If there are any questions or concerns regarding the setup, please email [contact@qfpl.io](mailto:contact@qfpl.io) with the details.