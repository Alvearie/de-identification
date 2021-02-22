/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.VINMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.RandomGenerators;
import com.ibm.whc.deid.util.VINManager;

public class VINMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -6153333908702391325L;

  protected static final char[] allowedCharacters =
      "ABCDEFGHJKLMNPRSTUVWXYZ0123456789".toCharArray();
  protected final boolean preserveWMI;
  protected final boolean preserveVDS;
  protected VINManager vinManager;
  protected final int unspecifiedValueHandling;
  protected final String unspecifiedValueReturnMessage;

  protected volatile boolean initialized = false;

  public VINMaskingProvider(VINMaskingProviderConfig configuration, String tenantId) {
    this.preserveWMI = configuration.isWmiPreserve();
    this.preserveVDS = configuration.isVdsPreserve();
    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  private String randomVIN() {
    String wmi = vinManager.getRandomWMI();
    String vds = RandomGenerators.randomUIDGeneratorWithInclusions(6, allowedCharacters);
    String vis = RandomGenerators.randomUIDGeneratorWithInclusions(8, allowedCharacters);
    return String.format("%s%s%s", wmi, vds, vis);
  }

  @Override
  public String mask(String identifier) {
    initialize();
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (!vinManager.isValidKey(identifier)) {
      debugFaultyInput("vinIdentifier");
      if (unspecifiedValueHandling == 2) {
        return this.randomVIN();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    }

    String wmi = identifier.substring(0, 3);
    String vds = null;
    String vis = RandomGenerators.randomUIDGeneratorWithInclusions(8, allowedCharacters);

    if (!this.preserveWMI) {
      wmi = vinManager.getRandomWMI(wmi);
    }

    if (!this.preserveVDS) {
      vds = RandomGenerators.randomUIDGeneratorWithInclusions(6, allowedCharacters);
    } else {
      vds = identifier.substring(3, 9);
    }

    return wmi + vds + vis;
  }

  protected void initialize() {
    if (!initialized) {
      vinManager = (VINManager) ManagerFactory.getInstance().getManager(null,
          Resource.WORLD_MANUFACTURERS_IDENTIFIER, null, localizationProperty);

      initialized = true;
    }
  }
}
