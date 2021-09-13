/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fpe;

public class PositionManager {

  protected enum CharType {
    DIGIT, UPPER, LOWER, OTHER
  }

  protected class Position {
    private char original;
    // TODO: keep?
    private char encrypted;
    private CharType type;

    public Position(char ch) {
      original = ch;
      encrypted = ch;
      if (ch >= '0' && ch <= '9') {
        type = CharType.DIGIT;
      } else if (ch >= 'a' && ch <= 'z') {
        type = CharType.LOWER;
      } else if (ch >= 'A' && ch <= 'Z') {
        type = CharType.UPPER;
      } else {
        type = CharType.OTHER;
      }
    }

    public char getEncrypted() {
      return encrypted;
    }

    public void setEncrypted(char encrypted) {
      this.encrypted = encrypted;
    }

    public char getOriginal() {
      return original;
    }

    public CharType getType() {
      return type;
    }
  }

  private Position[] positions;

  public PositionManager(String original) {
    int len = original.length();
    positions = new Position[len];
    for (int i = 0; i < len; i++) {
      positions[i] = new Position(original.charAt(i));
    }
  }

  public String extract(boolean includeDigits, boolean includeLower, boolean includeUpper) {
    int length = positions.length;
    StringBuilder buffer = new StringBuilder(length);
    for (Position p : positions) {
      switch (p.getType()) {
        case DIGIT:
          if (includeDigits) {
            buffer.append(p.getOriginal());
          }
          break;
        case LOWER:
          if (includeLower) {
            buffer.append(p.getOriginal());
          }
          break;
        case UPPER:
          if (includeUpper) {
            buffer.append(p.getOriginal());
          }
          break;
        default:
          // nothing required for other types
          break;
      }
    }
    return buffer.toString();
  }

  public String replaceSymbols(String encrypted, boolean includeDigits, boolean includeLower,
      boolean includeUpper) {
    int encryptedIndex = 0;
    StringBuilder buffer = new StringBuilder(encrypted.length());
    for (Position p : positions) {
      switch (p.getType()) {
        case DIGIT:
          buffer.append(includeDigits ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        case LOWER:
          buffer.append(includeLower ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        case UPPER:
          buffer.append(includeUpper ? encrypted.charAt(encryptedIndex++) : p.getOriginal());
          break;
        default:
          buffer.append(p.getOriginal());
          break;
      }
    }
    return buffer.toString();
  }
}
