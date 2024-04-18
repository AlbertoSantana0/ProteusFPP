# FPP + PROTEUS Compiler

## Project Overview

This project integrates FPP (F Prime Prime) and Proteus into a unified compiler. FPP is a modeling language designed specifically for the F Prime flight software framework, while Proteus is a programming language tailored for authoring actor-based hierarchical state machines.

## Key Features

- **Tokenizer and Parser**: The core of this compiler consists of a tokenizer and a parser that can process files containing FPP or Proteus code. It identifies and allocates each pattern of the code to its respective language FPP code is grouped with FPP, and Proteus code with Proteus.

## How It Works

The compiler operates by scanning files for patterns that match FPP or Proteus syntax. Upon detection, it categorizes and processes these patterns according to the language specifications. This separation allows for targeted parsing strategies that enhance the efficiency and accuracy of the compiler.

## Getting Started

To use this compiler, follow these simple steps:

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/AlbertoSantana0/ProteusFPP.git

2. **SBT (Simple Build Tool)**:
Essential for building Scala projects. Follow the instructions below to install SBT.

### Installing SBT

SBT (Simple Build Tool) is essential for building Scala projects. Follow the steps below to install SBT on your system:

#### General Installation with Coursier

1. **Install Scala and SBT using Coursier**:
   - Follow the installation guide on the [Coursier install page](https://get-coursier.io/docs/cli-installation).
   - Run the following command to set up your environment:
     ```bash
     cs setup
     sbt --script-version
     ```
   - This setup will install the latest stable version of SBT.

#### Platform-Specific Installation

- **macOS**:
  - Use Homebrew:
    ```bash
    brew install sbt
    ```

- **Windows and Linux**:
  - Use SDKMAN!:
    ```bash
    sdk install sbt
    ```

### Setting Up a Simple Hello World Project

1. **Create a New Project**:
   - Make a new directory for your project and navigate into it.
   - Create a `build.sbt` file with the following content:
     ```
     name := "HelloWorld"
     version := "0.1"
     scalaVersion := "2.13.6"
     ```

### Using SBT Shell

1. **Start SBT Shell**:
   Navigate to your project directory and run:
   ```bash
   sbt
2. **Compile Your Project**:
   Inside the sbt shell, type: 
   ```
   compile
     ```
3. **Run Your Project**: 
Inside the sbt shell, type: 
   ```
   run
     ```
4. **Exit SBT SHELL**:
Type `exit` or use `Ctrl+D` (Unix) or `Ctrl+Z` (Windows) to leave the sbt shell.
