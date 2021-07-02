/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import eu.internetofus.common.model.ModelTestCase;

/**
 * Test the {@link APIInfo}.
 *
 * @see APIInfo
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class APIInfoTest extends ModelTestCase<APIInfo> {

  /**
   * {@inheritDoc}
   */
  @Override
  public APIInfo createModelExample(final int index) {

    final var version = new APIInfo();
    version.name = "name" + index;
    version.apiVersion = "0.0." + index;
    version.softwareVersion = "0." + index + ".0";
    version.vendor = "vendor" + index;
    version.license = "license" + index;
    return version;
  }

}
