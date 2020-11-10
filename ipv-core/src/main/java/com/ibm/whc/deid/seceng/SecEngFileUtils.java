/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.seceng;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/** File utilities for Secure Engineering. */
public class SecEngFileUtils {
  public enum FileOperation {
    Read, Write, ReadWrite, WorkingDir
  };

  /**
   * Gets the file from the directory and the file name, including doing any path traversal checking
   * required by Secure Engineering.
   *
   * @param dir the directory
   * @param name the file name
   * @return the file
   * @throws IOException
   */
  public static File getFile(File dir, String name) throws IOException {
    if (name == null || name.isEmpty())
      return null;

    final File theFile = new File(dir, name);
    validateFile(theFile, FileOperation.ReadWrite);
    return theFile;
  }

  /**
   * Gets the file from the directory name and the file name, including doing any path traversal
   * checking required by Secure Engineering.
   *
   * @param dir the directory name
   * @param name the file name
   * @return the file
   * @throws IOException
   */
  public static File getFile(String dir, String name) throws IOException {
    File validDir = validateAndResolve(dir, FileOperation.ReadWrite);
    final File theFile = new File(validDir, name);
    validateFile(theFile, FileOperation.ReadWrite);
    return theFile;
  }

  /**
   * Gets the file from the file name, including doing any path traversal checking required by
   * Secure Engineering.
   *
   * @param name the file name
   * @return the file
   * @throws IOException
   */
  public static File getFile(String name) throws IOException {
    final File theFile = validateAndResolve(name, FileOperation.ReadWrite);
    return theFile;
  }

  /**
   * Gets the file from the directory and the file name for the specified file operation, including
   * doing any path traversal checking required by Secure Engineering.
   *
   * @param dir the directory
   * @param name the file name
   * @param op the file operation
   * @return the file
   * @throws IOException
   */
  public static File getFile(File dir, String name, FileOperation op) throws IOException {
    if (name == null || name.isEmpty())
      return null;

    final File theFile = new File(dir, name);
    validateFile(theFile, op);
    return theFile;
  }

  /**
   * Gets the file from the directory name and the file name for the specified file operation,
   * including doing any path traversal checking required by Secure Engineering.
   *
   * @param dir the directory
   * @param name the file name
   * @param op the file operation
   * @return the file
   * @throws IOException
   */
  public static File getFile(String dir, String name, FileOperation op) throws IOException {
    File validDir = validateAndResolve(dir, op);
    final File theFile = new File(validDir, name);
    validateFile(theFile, op);
    return theFile;
  }

  /**
   * Gets the file from the file name for the specified file operation, including doing any path
   * traversal checking required by Secure Engineering.
   *
   * @param name the file name
   * @param op the file operation
   * @return the file
   * @throws IOException
   */
  public static File getFile(String name, FileOperation op) throws IOException {
    final File theFile = validateAndResolve(name, op);
    return theFile;
  }

  /**
   * Gets the file from the file for the specified file operation, including doing any path
   * traversal checking required by Secure Engineering.
   *
   * @param theFile the file
   * @param op the file operation
   * @return the file
   * @throws IOException
   */
  public static File getFile(File theFile, FileOperation op) throws IOException {
    validateFile(theFile, op);
    return theFile;
  }

  /**
   * Gets the file from the file, including doing any path traversal checking required by Secure
   * Engineering.
   *
   * @param theFile the file
   * @return the file
   * @throws IOException
   */
  public static File getFile(final File theFile) throws IOException {
    validateFile(theFile, FileOperation.ReadWrite);
    return theFile;
  }

  /**
   * Creates a temporary file in the specified directory, including doing any path traversal
   * checking required by Secure Engineering.
   *
   * @param prefix the file name prefix
   * @param suffix the file name suffix
   * @param directory the directory
   * @return the file
   * @throws IOException
   */
  public static File createTempFile(String prefix, String suffix, File directory)
      throws IOException {
    if (directory != null) {
      validateTempFileParams(prefix, suffix, directory);
      final Path dirPath = directory.toPath().normalize();
      validatePath(dirPath, FileOperation.Write);

      final File tempFilePath = File.createTempFile(prefix, suffix, dirPath.toFile());
      validateFile(tempFilePath, FileOperation.Write);
      return tempFilePath;
    } else {
      return createTempFile(prefix, suffix);
    }
  }

  /**
   * Creates a temporary file, including doing any path traversal checking required by Secure
   * Engineering.
   *
   * @param prefix the file name prefix
   * @param suffix the file name suffix
   * @return the file
   * @throws IOException
   */
  public static File createTempFile(String prefix, String suffix) throws IOException {
    validateTempFileParams(prefix, suffix, null);
    final File theFile = File.createTempFile(prefix, suffix);
    validateFile(theFile, FileOperation.Write);
    return theFile;
  }

  /**
   * Validates the file for the specified file operation, including doing any path traversal
   * checking required by Secure Engineering.
   *
   * @param checkFile the file
   * @param op the file operation
   * @throws IOException
   */
  public static void validateFile(File checkFile, FileOperation op) throws IOException {
    if (checkFile == null) {
      throw new NullPointerException("Missing FilePath");
    }
    validatePath(checkFile.toPath().normalize(), op);
  }

  /**
   * Validates the file path for the specified file operation, including doing any path traversal
   * checking required by Secure Engineering.
   *
   * @param filePath the file path
   * @param op the file operation
   * @return the validated path
   * @throws IOException
   */
  public static Path validatePath(Path filePath, FileOperation op) throws IOException {
    if (filePath == null) {
      throw new NullPointerException("Missing FilePath");
    }
    if (op == null) {
      throw new NullPointerException("Missing FileOperation");
    }

    // Make sure path is normalized
    if (!filePath.isAbsolute() && filePath.getNameCount() > 1) {
      filePath = filePath.normalize();
    }

    return filePath;
  }

  private static void validateTempFileParams(String prefix, String suffix, File dir)
      throws IOException {
    if (prefix != null) {
      if (prefix.contains("..")) {
        throw new IOException();
      }
    }
    if (dir != null)
      validateFile(dir, FileOperation.Write);
  }

  private static File validateAndResolve(String dir, FileOperation op) throws IOException {
    if (dir == null) {
      throw new NullPointerException("Missing FilePath");
    }
    Path path = Paths.get(dir);
    validatePath(path, op);
    return path.toFile();
  }
}
