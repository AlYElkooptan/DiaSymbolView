# Compiling Instructions

To compile the project, you can use the free [Community Edition of Embarcadero Delphi](https://www.embarcadero.com/products/delphi/starter). There are, however, some extra steps since the tool uses custom UI components which need to be registered in the IDE before it can open the project:

1. Clone the repository **and its submodule dependencies** using `git clone --recurse-submodules`. Alternatively, you can use `git submodule update --init --recursive` after cloning the repository.
2. Make sure there are files under the `.\NtUtilsUI\NtUtils` directory, otherwise, you didn't clone all submodules.
3. Install `VirtualTree for VCL` using the IDE menu `Tools` -> `GetIt Package Manager`.
![VirtualTree](https://user-images.githubusercontent.com/30962924/180660667-43aa9113-ccc6-4548-8a94-9f81ed84e8eb.png)
4. Open `.\NtUtilsUI\Components\VirtualTreesExtension.dproj` in the IDE and click `Install` on the project to register it as a design-time package.    
![Install](https://user-images.githubusercontent.com/30962924/180660721-50fe47dc-039d-40cf-a190-c99f91ac0e2d.png)
5. Similarly, open `.\NtUtilsUI\VclEx\VclExtension.dproj` and click `Install`.
6. You can now open the main `.dproj` file and build the project from there.

Optionally, if you also want to generate a debug symbols during compilation, you'll need [map2pdb](https://github.com/andersmelander/map2pdb), which can convert Delphi-generated `.map` into a `.pdb` and attach it to the compiled executable. The project is already configured for generating `*.map` files and using a post-build event for their conversion into `.pdb`, so you can download map2pdb and place it somewhere where the Delphi compiler can find and invoke it.
