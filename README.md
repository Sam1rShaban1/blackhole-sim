# Gargantua: A Black Hole Simulation in Haskell

![Made with Haskell](https://img.shields.io/badge/Made%20with-Haskell-orange.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)

This project is a real-time 3D simulation of a "Gargantua"-style black hole, heavily inspired by the visuals from the film *Interstellar*. It is written entirely in Haskell using the OpenGL bindings provided by the `GLUT` library.

The simulation uses a particle system to render a flowing accretion disk, faking the gravitational lensing effect to create the iconic "halo" of light above and below the event horizon.

## Preview

<img width="1194" height="895" alt="Screenshot 2025-09-25 004338" src="https://github.com/user-attachments/assets/8cab8a8e-0a4b-4116-8187-e529fb5ae6a5" />


## Features

-   **Flowing Accretion Disk**: Tens of thousands of particles spiral inwards towards the black hole, simulating the flow of matter.
-   **Gravitational Lensing Effect**: The iconic "halo" is faked by rendering a second, vertical disk of particles, creating the illusion that light from the back of the disk is being bent over the top.
-   **Particle Recycling System**: To create a continuous flow, particles that fall past the event horizon are "reborn" at the outer edge of the disk.
-   **Relativistic Beaming (Doppler Effect)**: The side of the disk spinning towards the camera appears brighter and slightly bluer, while the side spinning away appears dimmer and redder.
-   **Realistic Color Gradient**: The accretion disk transitions from a brilliant, hot white near the event horizon, to a rich yellow/gold, and finally to a dusty orange at the outer edge.
-   **Photon Sphere Ring**: A sharp, bright white ring is rendered around the black hole's shadow to define the event horizon.
-   **Cinematic Camera**: The camera slowly orbits the scene to provide a dynamic view.
-   **Performance Monitoring**: An on-screen FPS counter helps monitor the simulation's performance.

## How It Works: The "Lensing" Trick

True gravitational lensing requires complex ray tracing. This simulation uses a common and effective trick to approximate the look:
1.  A primary, flat **horizontal disk** of particles is created, orbiting the center.
2.  A secondary **vertical disk** of particles is also created, orbiting on a plane perpendicular to the first.
3.  When viewed from an edge-on angle, the vertical disk appears as a "halo" of light passing over the top and bottom of the black hole, perfectly mimicking the look of lensed light.

## Requirements

1.  **GHC (The Glasgow Haskell Compiler)**: The standard Haskell compiler. We recommend installing it via [GHCup](https://www.haskell.org/ghcup/).
2.  **Cabal**: The Haskell build tool, which is included with GHCup.
3.  **OpenGL/GLUT Libraries**: You need the C libraries for GLUT on your system.

    -   **On Debian/Ubuntu**:
        ```bash
        sudo apt-get update
        sudo apt-get install freeglut3-dev
        ```
    -   **On Fedora/CentOS/RHEL**:
        ```bash
        sudo dnf install freeglut-devel
        ```
    -   **On macOS** (with [Homebrew](https://brew.sh/)):
        ```bash
        brew install freeglut
        ```
    -   **On Windows**: GLUT is often handled by Haskell's build tools, but if you have issues, you may need to install it with a package manager like `msys2` or `vcpkg`.

## Build and Run

1.  **Save the Code**: Save the final Haskell script as `Gargantua.hs` (or any other name).

2.  **Install Haskell Dependencies**: Open your terminal in the project directory and use Cabal to install the necessary packages.
    ```bash
    # Update your local package index
    cabal update

    # Install the GLUT and random packages
    cabal install GLUT random
    ```

3.  **Compile the Program**: Use GHC to compile the script. The `-O2` flag enables optimizations, which is highly recommended for graphical applications.
    ```bash
    ghc -O2 --make Gargantua.hs -o Gargantua
    ```
    *(If you saved the file with a different name, replace `Gargantua.hs` accordingly.)*

4.  **Run the Simulation**: Execute the compiled binary.
    ```bash
    ./Gargantua
    ```

## Customization

You can easily tweak the simulation's appearance by changing the constants at the top of the `Gargantua.hs` file.

-   `numDiskParticles`, `numHaloParticles`: Increase or decrease to affect density and performance.
-   `diskRadiusMin`, `diskRadiusMax`: Change the size of the accretion disk.
-   `threadDelay`: Modify the value in the `idle` function to change the target frame rate.

## License

This project is licensed under the MIT License. See the LICENSE file for details.
