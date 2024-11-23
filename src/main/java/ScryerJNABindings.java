import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import java.lang.ref.Cleaner;
import java.util.Optional;

public class ScryerJNABindings {

    private final ScryerProlog binding;

    public interface ScryerProlog extends Library {
        Pointer scryer_machine_new();

        void scryer_machine_free(Pointer ptr);

        Pointer scryer_run_query_iter(Pointer machine, Pointer input);

        void scryer_query_state_free(Pointer query_state);

        Pointer scryer_run_query_next(Pointer query_state);

        void scryer_consult_module_string(Pointer machine, Pointer module_name, Pointer input);

        Pointer scryer_run_query(Pointer machine, Pointer input);

        void scryer_free_c_string(Pointer ptr);
    }



    public ScryerJNABindings(String libraryPath) {
        this.binding = Native.load(libraryPath, ScryerProlog.class);
    }

    public void freeCString(Pointer ptr) {
        binding.scryer_free_c_string(ptr);
    }


    public class ScryerMachine implements AutoCloseable {
        private final Pointer machine;
        private final ScryerProlog binding;
        private final Cleaner.Cleanable cleanable;

        private Pointer query_state;

        private static class CleaningAction implements Runnable {
            private final ScryerProlog binding;
            private final Pointer machine;

            public CleaningAction(ScryerProlog binding, Pointer machine) {
                this.binding = binding;
                this.machine = machine;
            }

            @Override
            public void run() {
                binding.scryer_machine_free(machine);
            }


        }

        /**
         * This class represents an iterator for executing Prolog queries using the ScryerProlog engine.
         * It implements the AutoCloseable interface to ensure proper resource cleanup.
         */
        public class ScryerPrologQueryIter implements AutoCloseable {
            private final ScryerMachine machine;

            public ScryerPrologQueryIter(ScryerMachine machine) {
                this.machine = machine;
            }

            public AutoFreeCString next() {
                if (machine.query_state == null) {
                    throw new IllegalStateException("No query generator running or query generator already closed!");
                }
                return machine.runQueryGeneratorStep();
            }

            @Override
            public void close() {
                machine.cleanupQueryGenerator() ;
            }
        }

        public ScryerMachine(ScryerProlog binding) {
            this.machine = binding.scryer_machine_new();
            this.binding = binding;
            Cleaner cleaner = Cleaner.create();
            this.cleanable = cleaner.register(this, new CleaningAction(binding, this.machine));
        }


        public Pointer startNewQueryGenerator(String input) {
            if (query_state != null) {
                throw new IllegalStateException("Query generator already running, ");
            }
            Pointer input1 = new Memory(Native.toByteArray(input, "UTF-8").length + 1);
            input1.setString(0, input, "UTF-8");
            this.query_state = binding.scryer_run_query_iter(machine, input1);
            return this.query_state;
        }

        public void cleanupQueryGenerator() {
            if (query_state == null) {
                throw new IllegalStateException("No query generator running");
            }
            binding.scryer_query_state_free(query_state);
            query_state = null;
        }

        public AutoFreeCString runQueryGeneratorStep() {

            Pointer ptr = binding.scryer_run_query_next(query_state);
            if (ptr == Pointer.NULL) {
                return null;
            }
            return new AutoFreeCString(binding, ptr);
        }



        public ScryerPrologQueryIter generativeQuery(String input) {
            if (query_state != null) {
                throw new IllegalStateException("Query generator already running");
            }
            this.startNewQueryGenerator(input);
            return new ScryerPrologQueryIter(this);
        }


        public void consultModuleString(String module_name, String input) {
            if (query_state != null) {
                throw new IllegalStateException("Query generator already running");
            }

            // Encode module_name to UTF-8 and allocate memory for it
            Memory module_name_memory = new Memory(Native.toByteArray(module_name, "UTF-8").length + 1);
            module_name_memory.setString(0, module_name, "UTF-8");

            // Encode input to UTF-8 and allocate memory for it
            Memory input_memory = new Memory(Native.toByteArray(input, "UTF-8").length + 1);
            input_memory.setString(0, input, "UTF-8");

            // Call the native method with encoded strings
            binding.scryer_consult_module_string(machine, module_name_memory, input_memory);
        }


        public AutoFreeCString runQuery(String input) {
            if (query_state != null) {
                throw new IllegalStateException("Query generator already running");
            }

            // Encode the input string to UTF-8 and allocate memory for it
            Memory input_memory = new Memory(Native.toByteArray(input, "UTF-8").length + 1);
            input_memory.setString(0, input, "UTF-8");

            // Call the native function with the encoded string
            Pointer result_ptr = binding.scryer_run_query(machine, input_memory);

            // Return the result wrapped in AutoFreeCString for automatic memory management
            return new AutoFreeCString(binding, result_ptr);
        }

        public void freeCString(Pointer ptr) {
            binding.scryer_free_c_string(ptr);
        }


        public Pointer getMachinePointer() {
            return this.machine;
        }

        @Override
        public void close() {
            cleanable.clean();
        }



    }

    public class AutoFreeCString implements AutoCloseable {

        private final Pointer ptr;
        private final ScryerProlog binding;

        public AutoFreeCString(ScryerProlog binding, Pointer ptr) {
            this.ptr = ptr;
            this.binding = binding;
        }

        public String getValue() {
            return ptr.getString(0);
        }

        public Pointer getPointer() {
            return ptr;
        }

        // Implement the AutoCloseable interface
        @Override
        public void close() {
            binding.scryer_free_c_string(ptr);
        }
    }

    public ScryerMachine getScryerMachine() {
        return new ScryerMachine(binding);
    }

    // idiomatic wrapper methods


    public static void main(String[] args) {

        ScryerJNABindings scryer = new ScryerJNABindings("/home/jay/programs/scryer-prolog/target/debug/libscryer_prolog.so");

        try (ScryerMachine machine = scryer.getScryerMachine()) {
            machine.consultModuleString("facts", ":- use_module(library(clpz)).");
//            machine.consultModuleString("blerps", "");
            String query = "3 #= 1+2.";
            try (AutoFreeCString result = machine.runQuery(query);) {
                System.out.println(result.getValue());
            }
            machine.consultModuleString("facts", "fact(1). fact(2). fact(3). fact(4). fact(5).");
            try (ScryerMachine.ScryerPrologQueryIter iter = machine.generativeQuery("fact(X).")) {
                AutoFreeCString result;
                while ((result = iter.next()) != null) {
                    System.out.println(result.getValue());
                }
            }
        }
    }
}