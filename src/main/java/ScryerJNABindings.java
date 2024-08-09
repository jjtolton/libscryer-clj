import com.sun.jna.*;
import com.sun.jna.ptr.PointerByReference;

public class ScryerJNABindings {

    public interface ScryerProlog extends Library {
        ScryerProlog INSTANCE = (ScryerProlog) Native.load(
                Platform.isWindows() ? "scryer_prolog.dll" : "/home/jay/programs/scryer-prolog/target/debug/libscryer_prolog.so",
                ScryerProlog.class);

        Pointer scryer_machine_new();
        void scryer_machine_free(Pointer ptr);

        Pointer scryer_start_new_query_generator(Pointer machine, String input);

        int scryer_cleanup_query_generator(Pointer machine, Pointer query_state);

        String scryer_run_query_generator(Pointer machine, Pointer query_state);

        int scryer_load_module_string(Pointer machine, String input);

        int scryer_consult_module_string(Pointer machine, String input);

        Pointer scryer_run_query(Pointer machine, String input);

        void scryer_free_c_string(Pointer ptr);
    }

    public static void main(String[] args) throws Exception {

        ScryerProlog binding = ScryerProlog.INSTANCE;

        // create a new machine
        Pointer machine = binding.scryer_machine_new();
        binding.scryer_consult_module_string(machine, ":- use_module(library(clpz)).");
        String query = "3 #= 1+2.";
        Pointer result = binding.scryer_run_query(machine, query);
        String resultString = result.getString(0);
        binding.scryer_free_c_string(result);
        System.out.println(resultString);
        binding.scryer_machine_free(machine);


    }
}