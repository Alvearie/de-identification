/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.List;
import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig.GeneralizeRule;
import com.ibm.whc.deid.utils.log.LogCodes;

public class GeneralizeMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 8003315754342350747L;

  private final List<GeneralizeRule> generalizeRuleSet;

  /**
   * Instantiates a new Generalize masking provider.
   *
   * <p>
   * (1) For all values of a data element which do not belong in set A, replace these values with
   * "Other". E.g., all patients' languages that are not listed as Spanish or English, are mapped to
   * "Other".
   *
   * <p>
   * (2) For all values of a data element which belong in set A, replace these values with a
   * specified value. (Creation of more general categories through the combination of rarer traits
   * into more general traits)
   *
   * <p>
   * Some examples below: Race: Combine specific races into a larger category of race. For example,
   * Vietnamese, Thai, and Japanese will be combined into a category of "Asian".
   *
   * <p>
   * Religion: Combine lesser practiced religions into a larger category of religion. For example,
   * Daoism, Shinto, and Confucianism will be combined into "Eastern Asian Religions".
   */
  public GeneralizeMaskingProvider() {
    this(new GeneralizeMaskingProviderConfig());
  }

  public GeneralizeMaskingProvider(GeneralizeMaskingProviderConfig configuration) {
    try {
      generalizeRuleSet = configuration.parseMaskRuleSet(configuration.getMaskRuleSet());
    } catch (Exception e) {
      // should not occur as validation has already checked done a parse
      String message = "maskRuleSet, ruleSet: " + configuration.getMaskRuleSet();
      log.logError(LogCodes.WPH2011E, e, message);
      throw new RuntimeException(message, e);
    }
  }

  /**
   * Masks the specified identifier to generalize categories based on a configured rule set.
   *
   * @param identifier
   * @return mask value
   */
  @Override
  public String mask(String identifier) {
    // If none of the rules apply, we return the original value.
    String maskValue = identifier;
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }
    if (generalizeRuleSet == null || generalizeRuleSet.isEmpty()) {
      // No masking rules provided.
      return null;
    }
    for (GeneralizeRule rule : generalizeRuleSet) {
      if (rule.getLogicalNegation()) {
        // We are processing a negated logic: the identifier is
        // replaced if it is not in the list of values defined.
        if (!rule.getValueSet().contains(identifier)) {
          maskValue = rule.getCategory();
          break; // we are done
        }
        // continue to the next rule. If there are no more rules,
        // then identifier is not replaced.
      } else {
        // We are processing the positive logic, the identifier is
        // replaced if it is in the list of values defined.
        if (rule.getValueSet().contains(identifier)) {
          maskValue = rule.getCategory();
          break; // we are done
        } else if (rule.getValueSet().size() == 1 && rule.getValueSet().contains("*")) {
          // this is the case where the value is replaced with
          // default (e.g., Other).
          maskValue = rule.getCategory();
          break; // we are done
        }
        // continue to the next rule. If there are no more rules,
        // then identifier is not replaced.
      }
    }
    return maskValue;
  }
}
