/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import java.util.Arrays;
import java.util.Collection;
import java.util.regex.Pattern;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;

public class IPAddressIdentifier extends AbstractRegexBasedIdentifier {
  /** */
  private static final long serialVersionUID = -3087224346830576701L;

  private static final String IPV4_ADDRESS_PATTERN =
      "^(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])$";
  private static final Pattern ipv4matcher = Pattern.compile(IPV4_ADDRESS_PATTERN);

  private static final String IPV6_ADDRESS_PATTERN =
      "^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$";
  private static final Pattern ipv6matcher = Pattern.compile(IPV6_ADDRESS_PATTERN);

  private static final Collection<Pattern> ipaddressPatterns =
      Arrays.asList(ipv4matcher, ipv6matcher);

  private static final String[] appropriateNames = {"IP Address", "IPAddress"};

  @Override
  public ProviderType getType() {
    return ProviderType.IP_ADDRESS;
  }

  @Override
  protected Collection<Pattern> getPatterns() {
    return ipaddressPatterns;
  }

  @Override
  public boolean isOfThisType(String data) {
    return isIPv4(data) || isIPv6(data);
  }

  @Override
  public String getDescription() {
    return "IP address identification. Supports both IPv4 and IPv6 addresses";
  }

  @Override
  public ValueClass getValueClass() {
    return ValueClass.TEXT;
  }

  /**
   * Is i pv 4 boolean.
   *
   * @param data the data
   * @return the boolean
   */
  public boolean isIPv4(String data) {
    return ipv4matcher.matcher(data).matches();
  }

  /**
   * Is i pv 6 boolean.
   *
   * @param data the data
   * @return the boolean
   */
  public boolean isIPv6(String data) {
    return ipv6matcher.matcher(data.toLowerCase()).matches();
  }

  @Override
  protected Collection<String> getAppropriateNames() {
    return Arrays.asList(appropriateNames);
  }
}
