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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.ArrayList;

import eu.internetofus.common.model.ModelTestCase;

/**
 * Test the {@link PublishedNormsPage}.
 *
 * @see PublishedNormsPage
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class PublishedNormPageTest extends ModelTestCase<PublishedNormsPage> {

  /**
   * {@inheritDoc}
   */
  @Override
  public PublishedNormsPage createModelExample(final int index) {

    final var model = new PublishedNormsPage();
    model.offset = index;
    model.total = 100 + index;
    model.norms = new ArrayList<>();
    model.norms.add(new PublishedNormTest().createModelExample(index));
    return model;
  }

}