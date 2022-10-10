/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import org.junit.Test;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionGroupModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionPossibleValueModel;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskRuleSetConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

public class MaskingProviderMetadataFactoryTest {

  private static final String[] COMMON_OPTIONS =
      {"unexpectedInputHandling", "unexpectedInputReturnMessage"};

  @Test
  public void testAllProvidersCovered() {
    for (MaskingProviderType provider : MaskingProviderType.values()) {
      MaskingProviderMetadataModel model =
          MaskingProviderMetadataFactory.getMaskingProviderMetadata(provider, Locale.US);
      if (provider == MaskingProviderType.FHIR_MORTALITY_DEPENDENCY) {
        // TODO: this provider is disabled pending removal or restructuring
        assertNull(model);
        continue;
      }
      assertNotNull("null metadata for provider " + provider.name(), model);
      assertEquals(provider.getIdentifier(), model.getName());
      assertNotNull(model.getDescription());
      assertNotNull(model.getDocLink());
      assertTrue("unexpected document link: " + model.getDocLink(),
          model.getDocLink().endsWith("#" + provider.getIdentifier().toLowerCase()));
      switch (provider) {
        case CONDITIONAL:
        case DATEDEPENDENCY:
        case DATETIME_CONSISTENT_SHIFT:
        case FHIR_MORTALITY_DEPENDENCY:
          assertFalse(model.isRulePreview());
          break;
        default:
          assertTrue(model.isRulePreview());
      }
      switch (provider) {
        case ADDRESS:
          verifyOptions(model, true, 9, null);
          break;
        case ATC:
          verifyOptions(model, true, 1, null);
          break;
        case BINNING:
          verifyOptions(model, true, 4, null);
          break;
        case CITY:
          verifyOptions(model, true, 3, null);
          break;
        case CONDITIONAL:
          verifyOptions(model, false, 2, null);
          break;
        case CONTINENT:
          verifyOptions(model, true, 2, null);
          break;
        case COUNTRY:
          verifyOptions(model, true, 3, null);
          break;
        case COUNTY:
          verifyOptions(model, true, 1, null);
          break;
        case CREDIT_CARD:
          verifyOptions(model, true, 1, null);
          break;
        case DATEDEPENDENCY:
          verifyOptions(model, true, 2, null);
          break;
        case DATETIME:
          verifyOptions(model, true, 1, new int[] {3, 2, 10, 3, 3, 5, 3, 3, 3, 7, 5});
          break;
        case DATETIME_CONSISTENT_SHIFT:
          verifyOptions(model, true, 6, null);
          break;
        case EMAIL:
          verifyOptions(model, true, 2, null);
          break;
        case FHIR_MORTALITY_DEPENDENCY:
          verifyOptions(model, false, 1, null);
          break;
        case FPE:
          verifyOptions(model, true, 4, null);
          break;
        case GENDER:
          verifyOptions(model, false, 0, null);
          break;
        case GENERALIZE:
          verifyOptions(model, false, 1, null);
          break;
        case GUID:
          verifyOptions(model, false, 0, null);
          break;
        case HASH:
          verifyOptions(model, false, 7, null);
          break;
        case HOSPITAL:
          verifyOptions(model, true, 1, null);
          break;
        case IBAN:
          verifyOptions(model, true, 1, null);
          break;
        case ICDV9:
          verifyOptions(model, true, 2, null);
          break;
        case ICDV10:
          verifyOptions(model, true, 2, null);
          break;
        case IMEI:
          verifyOptions(model, true, 1, null);
          break;
        case IP_ADDRESS:
          verifyOptions(model, true, 1, null);
          break;
        case LATITUDE_LONGITUDE:
          verifyOptions(model, true, 5, null);
          break;
        case MAC_ADDRESS:
          verifyOptions(model, true, 1, null);
          break;
        case MAINTAIN:
          verifyOptions(model, false, 0, null);
          break;
        case MARITAL:
          verifyOptions(model, true, 0, null);
          break;
        case NAME:
          verifyOptions(model, true, 3, null);
          break;
        case NULL:
          verifyOptions(model, false, 1, null);
          break;
        case NUMBERVARIANCE:
          verifyOptions(model, true, 7, null);
          break;
        case OCCUPATION:
          verifyOptions(model, true, 1, null);
          break;
        case PHONE:
          verifyOptions(model, true, 3, null);
          break;
        case PSEUDONYM:
          verifyOptions(model, false, 0, new int[] {7, 4, 2});
          break;
        case RACE:
          verifyOptions(model, true, 0, null);
          break;
        case RANDOM:
          verifyOptions(model, false, 0, null);
          break;
        case REDACT:
          verifyOptions(model, false, 2, null);
          break;
        case RELIGION:
          verifyOptions(model, true, 0, null);
          break;
        case REPLACE:
          verifyOptions(model, false, 4, null);
          break;
        case SSN_UK:
          verifyOptions(model, true, 1, null);
          break;
        case SSN_US:
          verifyOptions(model, true, 2, null);
          break;
        case STATE_US:
          verifyOptions(model, true, 0, null);
          break;
        case SWIFT:
          verifyOptions(model, true, 1, null);
          break;
        case URL:
          verifyOptions(model, true, 5, null);
          break;
        case VIN:
          verifyOptions(model, true, 2, null);
          break;
        case ZIPCODE:
          verifyOptions(model, true, 11, null);
          break;
        default:
          fail("provider " + provider.name() + " not handled");
      }
    }
  }

  private void verifyOptions(MaskingProviderMetadataModel model, boolean commonSupported,
      int optionCount, int[] groupCounts) {
    int count = optionCount;
    if (commonSupported) {
      count += COMMON_OPTIONS.length;
    }
    if (count == 0) {
      assertNull(model.getOptions());
    } else {
      int optionsSize = model.getOptions().size();
      assertEquals(count, optionsSize);
      verifyOptions(model.getOptions());
      if (commonSupported) {
        for (int index = 0; index < COMMON_OPTIONS.length; index++) {
          ConfigurationOptionModel option = model.getOptions().get(optionsSize - COMMON_OPTIONS.length + index);
          assertEquals(COMMON_OPTIONS[index], option.getOptionName());
          if (index == 0) { // unexpectedInputHandling
            assertEquals(OptionType.String, option.getOptionType());
            assertEquals("NULL", option.getOptionDefault());
            assertNull(option.getExcluded());
            assertEquals(4, option.getPossibleValues().size());
            assertEquals("NULL", option.getPossibleValues().get(0).getValue());
            assertNotNull(option.getPossibleValues().get(0).getValueDisplayName());
            assertEquals("RANDOM", option.getPossibleValues().get(1).getValue());
            assertNotNull(option.getPossibleValues().get(1).getValueDisplayName());
            assertEquals("MESSAGE", option.getPossibleValues().get(2).getValue());
            assertNotNull(option.getPossibleValues().get(2).getValueDisplayName());
            assertEquals("ERROR_EXIT", option.getPossibleValues().get(3).getValue());
            assertNotNull(option.getPossibleValues().get(3).getValueDisplayName());
          } else if (index == 1) { // unexpectedInputReturnMessage
            assertEquals(OptionType.String, option.getOptionType());
            assertEquals("OTHER", option.getOptionDefault());
            assertNull(option.getExcluded());
            assertNull(option.getPossibleValues());
          }
        }
      }
    }
    if (groupCounts == null) {
      assertNull(model.getOptionGroups());
    } else {
      assertEquals(groupCounts.length, model.getOptionGroups().size());
      for (int index = 0; index < groupCounts.length; index++) {
        ConfigurationOptionGroupModel groupModel = model.getOptionGroups().get(index);
        assertEquals("wrong number of options in group " + index, groupCounts[index],
            groupModel.getGroupOptions().size());
        assertNotNull(groupModel.getGroupName());
        assertNotNull(groupModel.getGroupDisplayName());
        verifyOptions(groupModel.getGroupOptions());
        index++;
      }
    }
  }

  private void verifyOptions(List<ConfigurationOptionModel> options) {
    Iterator<ConfigurationOptionModel> it = options.iterator();
    while (it.hasNext()) {
      ConfigurationOptionModel opt = it.next();
      assertNotNull(opt.getOptionName());
      if (!(opt instanceof MaskRuleSetConfigurationOptionModel)) {
        assertNotNull(opt.getOptionDisplayName());
        assertNotNull(opt.getOptionDescription());
      }
      assertNotNull(opt.getOptionType());
      if (opt.getPossibleValues() != null) {
        // if not null, should not be empty
        assertFalse(opt.getPossibleValues().isEmpty());
        Iterator<ConfigurationOptionPossibleValueModel> pvit = opt.getPossibleValues().iterator();
        while (pvit.hasNext()) {
          ConfigurationOptionPossibleValueModel pv = pvit.next();
          assertNotNull(pv.getValue());
          assertNotNull(pv.getValueDisplayName());
        }
      }
    }
  }
}
