/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.List;
import org.junit.Ignore;
import org.junit.Test;
import com.ibm.whc.deid.providers.identifiers.Identifier;
import com.ibm.whc.deid.providers.identifiers.NameIdentifier;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class NameMaskingProviderTest extends TestLogSetUp implements MaskingProviderTest {

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testMask() throws Exception {
    NameMaskingProviderConfig config = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(config, tenantId, localizationProperty);

    doTestChanged(nameMaskingProvider, "John");

    // capitalization shouldn't matter
    doTestChanged(nameMaskingProvider, "JoHn");
  }

  private String doTestChanged(NameMaskingProvider nameMaskingProvider, String input) {
    String res = null;
    boolean changed = false;
    for (int i = 0; i < 10; i++) {
      res = nameMaskingProvider.mask(input);
      assertNotNull(res);
      assertFalse(res.isEmpty());
      if (!input.equals(res)) {
        changed = true;
        break;
      }
    }
    assertTrue(changed);
    return res;
  }

  @Test
  public void testMaskGender() throws Exception {
    String testProp = "/localization/test.name.localization.properties";
    List<String> femaleNames = Arrays.asList("Veronica", "Michelle", "Tanya");
    List<String> maleNames =
        Arrays.asList("Roger", "Jarrett", "Dumas", "Denis", "Mark", "Rafat", "Bobby");
    List<String> lastNames = Arrays.asList("Anderson", "Stevenson", "Holm", "Rodriguez", "Stewart");
    List<String> unisexNames = Arrays.asList("Chris", "Alice");

    NameMaskingProviderConfig config = new NameMaskingProviderConfig();
    config.setMaskGenderPreserve(true);
    config.setMaskingAllowUnisex(false);
    config.setUnspecifiedValueHandling(3);
    NameMaskingProvider provider = new NameMaskingProvider(config, tenantId, testProp);

    assertEquals("OTHER", provider.mask("John"));

    boolean changed = false;
    for (int i = 0; i < 20; i++) {
      String name = provider.mask("Tanya");
      assertNotNull(name);
      assertTrue(femaleNames.contains(name));
      if (!"Tanya".equals(name)) {
        changed = true;
      }
    }
    assertTrue(changed);

    changed = false;
    for (int i = 0; i < 20; i++) {
      String name = provider.mask("Holm");
      assertNotNull(name);
      assertTrue(lastNames.contains(name));
      if (!"Holm".equals(name)) {
        changed = true;
      }
    }
    assertTrue(changed);

    changed = false;
    for (int i = 0; i < 20; i++) {
      String name = provider.mask("Roger");
      assertNotNull(name);
      assertTrue(maleNames.contains(name));
      if (!"Roger".equals(name)) {
        changed = true;
      }
    }
    assertTrue(changed);

    config.setMaskingAllowUnisex(true);
    provider = new NameMaskingProvider(config, tenantId, testProp);

    changed = false;
    for (int i = 0; i < 20; i++) {
      String name = provider.mask("Roger");
      assertNotNull(name);
      assertTrue(maleNames.contains(name) || unisexNames.contains(name));
      if (!"Roger".equals(name)) {
        changed = true;
      }
    }
    assertTrue(changed);
  }

  @Test
  public void testMaskNameAndSurname() throws Exception {
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(defaultConfiguration, tenantId, localizationProperty);

    String name = "John Smith";
    String res = nameMaskingProvider.mask(name);
    assertFalse(name.equals(res));
    String[] replacements = res.split(" ");
    assertEquals(2, replacements.length);
    assertTrue(nameMaskingProvider.getNamesManager().isFirstName(replacements[0]));
    assertTrue(nameMaskingProvider.getNamesManager().isLastName(replacements[1]));
  }

  @Test
  @Ignore
  public void testPerformance() {
    int N = 1000000;
    NameMaskingProviderConfig defaultConfiguration = new NameMaskingProviderConfig();
    NameMaskingProviderConfig unisexConfiguration = new NameMaskingProviderConfig();
    unisexConfiguration.setMaskingAllowUnisex(true);

    NameMaskingProviderConfig[] configurations =
        new NameMaskingProviderConfig[] {defaultConfiguration, unisexConfiguration};

    String[] originalValues = new String[] {"John"};

    for (NameMaskingProviderConfig maskingConfiguration : configurations) {
      NameMaskingProvider maskingProvider =
          new NameMaskingProvider(maskingConfiguration, tenantId, localizationProperty);

      for (String originalValue : originalValues) {
        long startMillis = System.currentTimeMillis();

        for (int i = 0; i < N; i++) {
          maskingProvider.mask(originalValue);
        }

        long diff = System.currentTimeMillis() - startMillis;
        System.out.println(String.format(" %s: %d operations took %d milliseconds (%f per op)",
            originalValue, N, diff, (double) diff / N));
        // Assert test always should finish in less than 10 seconds
        assertTrue(diff < 10000);
      }
    }
  }

  @Test
  public void testMaskUnisexAllowed() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskingAllowUnisex(true);

    NameMaskingProvider nameMaskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    doTestChanged(nameMaskingProvider, "Mary");
  }

  @Test
  public void testMaskNullNameInputReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskingAllowUnisex(true);
    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = null;
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(1);

    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnRandom() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(2);
    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);
    Identifier identifier = new NameIdentifier(tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertFalse(maskedName.equals(invalidName));
    assertTrue(identifier.isOfThisType(maskedName));
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnDefaultCustomValue() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals("OTHER", maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputValidHandlingReturnNonDefaultCustomValue() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(3);
    configuration.setUnspecifiedValueReturnMessage("Test Name");
    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals("Test Name", maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void testMaskInvalidNameInputInvalidHandlingReturnNull() throws Exception {
    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setUnspecifiedValueHandling(4);
    MaskingProvider maskingProvider =
        new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String invalidName = "Invalid Name";
    String maskedName = maskingProvider.mask(invalidName);

    assertEquals(null, maskedName);
    assertThat(outContent.toString(), containsString("DEBUG - WPH1015D"));
  }

  @Test
  public void pseudoRandom() throws Exception {
    String original1 = "John Doe";
    String original2 = "John James Smith";

    NameMaskingProviderConfig configuration = new NameMaskingProviderConfig();
    configuration.setMaskPseudorandom(true);

    MaskingProvider mp = new NameMaskingProvider(configuration, tenantId, localizationProperty);

    String maskedName1 = mp.mask(original1);
    String maskedName2 = mp.mask(original2);

    assertEquals(maskedName1.split(" ")[0], maskedName2.split(" ")[0]);

    // repeat calls, same results
    assertEquals(maskedName1, mp.mask(original1));
    assertEquals(maskedName2, mp.mask(original2));
  }
}
