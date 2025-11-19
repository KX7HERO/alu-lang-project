module Run

import Main;
import IO;

public void main(list[str] args) {
    println("Starting Run...");
    Main::main([|file:///workspaces/alu-lang-project/examples/simple.alu|]);
    println("Finished Run.");
}
