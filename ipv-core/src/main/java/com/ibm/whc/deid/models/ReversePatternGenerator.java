/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.models;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A parsable reverse-regex pattern. Patterns use a form of reverse regex, see constructor for
 * details
 *
 */
public class ReversePatternGenerator extends ReverseRegexParser implements TokenSet, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -3462702493773063784L;
// Country code for this pattern
  private final String countryCode;
  // A random number generator
  private final SecureRandom random = new SecureRandom();
  // The set of steps to construct a string of this pattern
  private List<BuildStep> buildSteps = new ArrayList<BuildStep>();

  /**
   * A pattern from a string containing reverse-regex, and a country code
   *
   * @param pattern The reverse regex of the string, in the following format
   * 
   *        <pre>
   * <code>
   * -Optional components have a ?.
   * -In all cases except in multiplicity, any character outside
   *   of this description is interpreted as a literal:
   *
   * Syntax Frames:
   * --------------
   * <b>[</b>	     ?	Begin character class. If using a single literal
   * 		  (\u1234, A, 1, etc) then the literal
   *   		  can be used instead of a full character class
   * 		  unless a specific class is desired.
   * 	x-y  ?	A character literal range to allow. Second index
   *         		  must be greater.
   * <b>]</b>		End of the class.
   *
   * <b>(</b>	     ?  Begin an optional class. These can be nested and
   * 		  include any other item.
   * 	|    ?	Separate optionals
   * <b>)</b>		Close optional class
   *
   * <b>{</b>	     ?  Begin multiplicity, if not present assume {1}
   * 	n1	Minimum number of this character class. Also maximum
   * 		  if the only argument.
   * 	, n2 ?	Maximum number of this character class
   * <b>}</b>		Close multiplicity
   *
   * Universal Escapes:
   * ------------------
   * \{		A literal character
   * \]
   * \[
   * \\
   * \-
   * \(
   * \)
   * \|
   *
   * \pHHHHHHHH	A UTF32 character. May be included in
   * 		  ranges (\pHHHHHHHH-\pHHHHHHHH)
   * \d		The set of Digits
   * \l 		The set of Lowercase ascii letters
   * \\u		The set of uppercase ascii letters (only use single \)
   *
   *  </code>
   *        </pre>
   *
   * @param countryCode The country code this pattern is used for.
   */
  public ReversePatternGenerator(String pattern, String countryCode) {
    // Split to unicode tokens
    super(new ArrayList<String>(Arrays.asList(pattern.split(""))));
    this.countryCode = countryCode;
    // Start parsing
    parsePattern();
  }

  /**
   * Parse a pattern by array. This is used internally for optionals
   *
   * @param pattern The pattern, separated in to token characters
   * @param countryCode The country code
   */
  public ReversePatternGenerator(List<String> pattern, String countryCode) {
    super(pattern);
    this.countryCode = countryCode;
    parsePattern();
  }

  /**
   * Pattern with no country associated.
   *
   * @param pattern the pattern
   */
  public ReversePatternGenerator(String pattern) {
    super(new ArrayList<String>(Arrays.asList(pattern.split(""))));
    this.countryCode = null;
    parsePattern();
  }

  /** Turn the pattern in to a list of build steps that can be used to construct it. */
  private void parsePattern() {
    BuildStep lastBuildStep = null;

    // Get the next class and multiplicity, and add it to the list
    lastBuildStep = nextBuildStep();
    while (lastBuildStep != null) {
      buildSteps.add(lastBuildStep);
      lastBuildStep = nextBuildStep();
    }
  }

  /**
   * Returns the next build step from the pattern string
   *
   * @return The next build step in the parse
   */
  private BuildStep nextBuildStep() {
    BuildStep bs = null;
    // For substrings of token classes, optionals and multiplicities
    // On first run, get the characters
    if (character == null) {
      getNewCharAndNewNextChar(true);
      // If still null, end of string and this build step is moot
      if (character == null) {
        return null;
      }
    }

    // A complex token class
    if (character.equals("[")) {
      bs = handleTokenClass();
    }
    // An optional class
    if (character != null && character.equals("(")) {
      bs = handleOptionClass();
    }
    // If no complex token class or optionals have been found, must be a
    // character literal or escape (or multiplicity,
    // handle multiplicities later)
    if (bs == null && character != null && !character.equals("{")) {
      // If this is an escape, get the next character and build a token
      // class from that
      if (character.equals("\\")) {
        getNewChar(true);
        if (character != null) {
          bs = new BuildStep(this.handleEscapes(), null);
        } else {
          // If the escape has no character after, this step is moot.
          return null;
        }
      }
      // If still null, must be a literal
      if (bs == null) {
        bs = new BuildStep(new TokenClass(new CharacterRange(character)), null);
      }
      getNewChar(true);
    }
    // If there is a build step from this iteration and there is a
    // multiplicity specified next
    if (character != null && character.equals("{") && bs != null) {
      if (nextCharacter == null) {
        return bs;
      }
      // put the multiplicity string in a list and use the Multiplicity
      // class to parse
      ArrayList<String> parseSeparate = new ArrayList<String>();
      while (!nextCharacter.equals("}")) {
        parseSeparate.add(nextCharacter);
        getNewChar(false);
        // If malformed, multiplicity is 1
        if (nextCharacter == null) {
          getNewCharAndNewNextChar(false);
          return bs;
        }
      }
      // Set the multiplicity. A null multiplicity is fine in the error
      // case
      // because this is implicitly 1 when constructing a string
      bs.setMultiplicity(new Multiplicity(this, parseSeparate));
      getNewCharAndNewNextChar(false);
    }
    return bs;
  }

  /**
   * Handle an optional by parsing the components as patterns
   *
   * @return A build step with null multiplicity, representing the optional
   */
  private BuildStep handleOptionClass() {
    // The build step to return and string to parse for the first option
    BuildStep bs = null;
    ArrayList<String> parseSeparate;
    getNewChar(false);

    // Number of brackets seen for nesting
    int bracketCount = 1;

    // The container for the options
    TokenClass tk = new TokenClass();
    parseSeparate = new ArrayList<String>();

    // Find the substring of the class match or fail
    while (true) {
      // Fail if no closing bracket before the end
      if (character == null) {
        return null;
      }
      // Treat escapes as literals since the string will be re-parsed.
      // This handles escaped brackets and such
      if (character.equals("\\")) {
        if (nextCharacter != null) {
          parseSeparate.add(character);
          parseSeparate.add(nextCharacter);
        }
        getNewCharAndNewNextChar(false);
        continue;
      }
      // On pipes within the current option set,
      if (character.equals("|") && bracketCount == 1) {
        // Finish this option
        if (parseSeparate.size() > 0) {
          tk.addCharacterSet(new ReversePatternGenerator(parseSeparate, this.countryCode));
        }
        parseSeparate = new ArrayList<>();
        getNewChar(false);
        continue;
      } else if (character.equals("(")) {
        // Nested brackets
        bracketCount++;
      } else if (character.equals(")")) {
        bracketCount--;
      }
      if (bracketCount > 0) {
        // Don't add the final bracket
        parseSeparate.add(character);
      } else {
        // We are done. Create and return a build step from the
        // substring, using a pattern.
        getNewChar(false);
        // Create a new build step. Multiplicity is null so implied 1
        ReversePatternGenerator p = new ReversePatternGenerator(parseSeparate, this.countryCode);
        tk.addCharacterSet(p);
        if (tk.getSize() > 0) {
          bs = new BuildStep(tk, null);
        }
        return bs;
      }
      getNewChar(false);
    }
  }

  /**
   * Parse a square bracket set to a token class
   *
   * @return The build step, multiplicity 1
   */
  private BuildStep handleTokenClass() {
    BuildStep bs = null;
    ArrayList<String> parseSeparate;
    // must have a matching ]
    if (nextCharacter == null) {
      getNewChar(false);
      return null;
    }
    parseSeparate = new ArrayList<String>();
    // Find the substring of the class match or fail
    while (!nextCharacter.equals("]")) {
      parseSeparate.add(nextCharacter);
      if (nextCharacter.equals("\\")) {
        getNewChar(false);
        if (nextCharacter != null) {
          parseSeparate.add(nextCharacter);
        }
      }
      getNewChar(false);
      if (nextCharacter == null) {
        getNewChar(false);
        return null;
      }
    }
    if (parseSeparate.size() > 0) {
      // Create a token class based on the substring
      TokenClass lastClassLoaded = new TokenClass(parseSeparate);
      // Create a new build step. Multiplicity is null so implied 1
      bs = new BuildStep(lastClassLoaded, null);
    }
    getNewCharAndNewNextChar(true);
    return bs;
  }

  @Override
  public String getRandomToken(SecureRandom s) {
    String returnString = "";
    // Get a string from each step, and add them together
    for (BuildStep bs : buildSteps) {
      String portion = bs.generateRandomString(s);
      if (portion != null) {
        returnString += portion;
      }
    }
    return returnString;
  }

  /** This class provides a random result for any index. */
  @Override
  public String getTokenAt(int index) {
    // Provide a random string, as there are no "Indexes" for this class
    return getRandomToken(random);
  }

  /** This class returns a size of 1 for any query. */
  @Override
  public int getSize() {
    return 1;
  }
}
