/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.Pad;
import com.ibm.whc.deid.shared.pojo.config.masking.FPEMaskingProviderConfig.UsageType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

public class FPEMaskingProviderConfigTest {

  @Test
  public void testMain() {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    assertEquals(Pad.NONE, config.getPadding());
    assertEquals(UsageType.DIGITS, config.getInputType());
    assertNull(config.getKey());
    assertNull(config.getTweak());
    assertEquals(MaskingProviderType.FPE, config.getType());

    FPEMaskingProviderConfig other = new FPEMaskingProviderConfig();
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setInputType(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_LOWER);
    assertEquals(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_LOWER, config.getInputType());
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setInputType(UsageType.DIGITS_LETTERS_INSENSITIVE_AS_LOWER);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setPadding(Pad.BACK);
    assertEquals(Pad.BACK, config.getPadding());
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setPadding(Pad.BACK);
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setKey("key2");
    assertEquals("key2", config.getKey());
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setKey("key2");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    config.setTweak("tweak2");
    assertEquals("tweak2", config.getTweak());
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());
    other.setTweak("tweak2");
    assertTrue(config.equals(other));
    assertEquals(config.hashCode(), other.hashCode());

    // verify superclass field impacts equals() and hashcode()
    config.setUnexpectedInputHandling(UnexpectedMaskingInputHandler.MESSAGE);
    assertFalse(config.equals(other));
    assertNotEquals(config.hashCode(), other.hashCode());

    config.setPadding(null);
    assertEquals(Pad.NONE, config.getPadding());

    config.setInputType(null);
    assertEquals(UsageType.DIGITS, config.getInputType());
  }

  @Test
  public void testValidation() throws InvalidMaskingConfigurationException {
    FPEMaskingProviderConfig config = new FPEMaskingProviderConfig();
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`key` must be 32, 48, or 64 characters", e.getMessage());
    }
    config.setKey("123456");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`key` must be 32, 48, or 64 characters", e.getMessage());
    }
    config.setKey("1234567890asdfghjklpoiiuytrewqas");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`key` must contain only characters 0-9 and a-f", e.getMessage());
    }
    config.setKey("1234567890ABCDEFabcdefabcde9876s");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`key` must contain only characters 0-9 and a-f", e.getMessage());
    }
    config.setKey("1234567890abcdefabcdefabcdef9999");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must be 16 characters", e.getMessage());
    }
    config.setTweak("1234567890abcdef");
    config.setKey("1234567890abcdefabcdefabcdef9999");
    config.validate(null);
    config.setKey("1234567890abcdefabcdefabcdef9999abcdefabcdef0987");
    config.validate(null);
    config.setKey("1234567890abcdefabcdefabcdef9999abcdefabcdefabcdefabcdef654398ea");
    config.validate(null);
    config.setKey("1234567890abcdefabcdefabcdef9999abcdefabcdefabcdefa88888888bcdef0");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`key` must be 32, 48, or 64 characters", e.getMessage());
    }

    config.setKey("1234567890abcdefabcdefabcdef9999abcdefabcdefabcdefabcdef654398ea");
    config.setTweak(null);
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must be 16 characters", e.getMessage());
    }
    config.setTweak("123456");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must be 16 characters", e.getMessage());
    }
    config.setTweak("1234567890asdfgh");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must contain only characters 0-9 and a-f", e.getMessage());
    }
    config.setTweak("1234567890ABCDEF");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must contain only characters 0-9 and a-f", e.getMessage());
    }
    config.setTweak("1234567890abcdef");
    config.validate(null);
    config.setTweak("1234567890abcdef0");
    try {
      config.validate(null);
      fail("expected exception");
    } catch (InvalidMaskingConfigurationException e) {
      assertEquals("`tweak` must be 16 characters", e.getMessage());
    }
    config.setTweak("0000000000000000");

    for (UsageType usage : UsageType.values()) {
      for (Pad pad : Pad.values()) {
        config.setPadding(pad);
        config.setInputType(usage);
        if ((usage == UsageType.DIGITS_LETTERS_SENSITIVE || usage == UsageType.LETTERS_SENSITIVE)
            && (pad == Pad.FRONT || pad == Pad.BACK)) {
          try {
            config.validate(null);
            fail("expected exception");
          } catch (InvalidMaskingConfigurationException e) {
            assertEquals("`padding` has the value " + pad.name() + " but the `inputType` value "
                + usage.name() + " does not support padding", e.getMessage());
          }
        } else {
          config.validate(null);
        }
      }
    }
  }
}
