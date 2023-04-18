/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.models.ReversePatternGenerator;
import java.security.SecureRandom;
import org.junit.Test;

public class ReversePatternGeneratorTest {
  private String countryCode = "US";
  private SecureRandom random = new SecureRandom();

  @Test
  public void testParseStaticPattern() {
    assertTrue(getPatternAndMatch("abc"));
  }

  @Test
  public void testParseStaticString() {
    // Test static string (dash is literal)
    assertTrue(getPatternAndMatch("a-z"));
    assertTrue(getPatternAndMatch("\\", ""));
  }

  @Test
  public void testDualCharacterClass() {
    assertTrue(getPatternAndMatch("[a-b][c-d]"));
    assertTrue(getPatternAndMatch("[a-b]{3}[c-d]"));
    assertTrue(getPatternAndMatch("[\\]]"));
    assertTrue(getPatternAndMatch("\\["));
    assertTrue(getPatternAndMatch("[\\", ""));
    assertTrue(getPatternAndMatch("[", ""));
    assertTrue(getPatternAndMatch("[a-b]{2,5}[c-d]{3}[e-f]{4,5}"));

    // Invalid character Classes
    // Invalid. Should return empty string or ignore the class
    assertTrue(getPatternAndMatch("[a-z", ""));
    assertTrue(getPatternAndMatch("abc[d-f", "abc"));
  }

  @Test
  public void testEscapes() {
    assertTrue(getPatternAndMatch("\\d{3}", "[0-9]{3}"));
    assertTrue(getPatternAndMatch("\\p00001234", "\u1234"));
  }

  @Test
  public void testOptionalClass() {
    assertTrue(getPatternAndMatch("([a-b]|[c-z])"));
    assertTrue(getPatternAndMatch("((a|b)|c|(d|e|[f-g]))"));
    assertTrue(getPatternAndMatch("((a{1}|b{1}){2}|c{2}){10}"));
    assertTrue(getPatternAndMatch("(d|e|f)abc"));
    assertTrue(getPatternAndMatch("(\\(|b)"));
    assertTrue(getPatternAndMatch("(a\\||b)"));
    assertTrue(getPatternAndMatch("(a|\\))"));

    // Invalid optional (Ignores the optional)
    assertTrue(getPatternAndMatch("(a|b", ""));
    assertTrue(getPatternAndMatch("((a|b)c", ""));
    assertTrue(getPatternAndMatch("(a|(b|c)", ""));
    assertTrue(getPatternAndMatch("abc(d|e|f", "abc"));
    assertTrue(getPatternAndMatch("(\\)", ""));
  }

  @Test
  public void testMultiplicity() {
    assertTrue(getPatternAndMatch("a{10}"));
    assertTrue(getPatternAndMatch("a{3,5}"));
    assertTrue(getPatternAndMatch("a{ 5, 6}", "a{5,6}"));
    assertTrue(getPatternAndMatch("((a|b){20}|c{10}){15}"));
    assertTrue(getPatternAndMatch("([a-b]{20}|([d-e]{3,5}|f){7}){10}"));

    // Invalid multiplicities, implies multiplicity of 1
    assertTrue(getPatternAndMatch("a{3", "a"));
    assertTrue(getPatternAndMatch("a{b}", "a"));
    assertTrue(getPatternAndMatch("a{1,b}", "a"));
    assertTrue(getPatternAndMatch("a{1,", "a"));
    assertTrue(getPatternAndMatch("a{c, ", "a"));
    System.out.println(getPatternByString("a{").getRandomToken(random));
    assertTrue(getPatternAndMatch("a{", "a"));
    assertTrue(getPatternAndMatch("([a-b]{1|c)d", "[abc]d"));
  }

  private ReversePatternGenerator getPatternByString(String pattern) {
    return new ReversePatternGenerator(pattern, countryCode);
  }

  private boolean getPatternAndMatch(String pattern, String regex) {
    return getPatternByString(pattern).getRandomToken(random).matches("^" + regex + "$");
  }

  private boolean getPatternAndMatch(String patternAndRegex) {
    return getPatternAndMatch(patternAndRegex, patternAndRegex);
  }
}
