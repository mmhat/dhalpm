// To be compiled with: gcc -lalpm -o setup-test-database setup-test-database.c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <alpm.h>

int setup_test_database(const char *root_directory, const char *db_directory, int package_count, char **packages) {
    alpm_handle_t *handle;
    alpm_pkg_t *pkg;
    alpm_errno_t err;
    alpm_list_t *data;

    // Initialize libalpm
    handle = alpm_initialize(root_directory, db_directory, &err);
    if (!handle) {
        fprintf(stderr, "Failed to initialize libalpm: %s\n", alpm_strerror(err));
        return EXIT_FAILURE;
    }

    if (package_count == 0) {
        alpm_release(handle);
        return EXIT_SUCCESS;
    }

    // Start a new transaction
    if (alpm_trans_init(handle, 0) < 0) {
        fprintf(stderr, "Failed to initialize transaction: %s\n", alpm_strerror(err));
        alpm_release(handle);
        return EXIT_FAILURE;
    }

    // Add each package to the transaction
    for (int i = 0; i < package_count; i++) {
        const char *package_path = packages[i];

        if (alpm_pkg_load(handle, package_path, 1, ALPM_SIG_PACKAGE_UNKNOWN_OK, &pkg) < 0) {
            fprintf(stderr, "Failed to load package: %s\n", package_path);
            continue;
        }

        if (alpm_add_pkg(handle, pkg) < 0) {
            fprintf(stderr, "Failed to add package to transaction: %s\n", package_path);
            alpm_pkg_free(pkg);
            continue;
        }
    }

    // Prepare the transaction
    if (alpm_trans_prepare(handle, &data) < 0) {
        fprintf(stderr, "Failed to prepare transaction: %s\n", alpm_strerror(err));
        alpm_trans_release(handle);
        alpm_release(handle);
        return EXIT_FAILURE;
    }

    // Commit the transaction
    if (alpm_trans_commit(handle, &data) < 0) {
        fprintf(stderr, "Failed to commit transaction: %s\n", alpm_strerror(err));
        alpm_trans_release(handle);
        alpm_release(handle);
        return EXIT_FAILURE;
    }

    // Clean up
    alpm_trans_release(handle);
    alpm_release(handle);
    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <root_directory> <database_directory> [<package_file>...]\n", argv[0]);
        fprintf(stderr, "  <root_directory>      The root directory.\n");
        fprintf(stderr, "  <database_directory>  The directory where the package databases are located.\n");
        fprintf(stderr, "  [<package_file>...]   Zero or more package files to be added to the transaction.\n");
        return EXIT_FAILURE;
    }

    return setup_test_database(argv[1], argv[2], argc - 3, &argv[3]);
}
