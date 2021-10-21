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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test the {@link InteractionsRepository}.
 *
 * @see InteractionsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class InteractionsRepositoryTest {

  /**
   * Verify that can not create interactions page sort.
   *
   * @param testContext context to test.
   *
   * @see InteractionsRepository#createInteractionsPageSort(List)
   */
  @Test
  public void shouldFailCreateInteractionsPageSort(final VertxTestContext testContext) {

    final List<String> order = new ArrayList<>();
    order.add("-undefinedKey");
    InteractionsRepository.createInteractionsPageSort(order)
        .onComplete(testContext.failing(any -> testContext.completeNow()));

  }

  /**
   * Verify that can not create interactions page sort.
   *
   * @param testContext context to test.
   *
   * @see InteractionsRepository#createInteractionsPageSort(List)
   */
  @Test
  public void shouldCreateInteractionsPageSort(final VertxTestContext testContext) {

    final List<String> order = new ArrayList<>();
    order.add("+taskTypeId");
    order.add("-appId");
    order.add("senderId");
    order.add("receiverId");
    order.add("-transactionLabel");
    order.add("transactionTs");
    order.add("-messageLabel");
    order.add("communityId");
    order.add("-taskId");
    order.add("messageTs");
    InteractionsRepository.createInteractionsPageSort(order)
        .onComplete(testContext.succeeding(sort -> testContext.verify(() -> {

          assertThat(sort).isNotNull();
          assertThat(sort.getInteger("appId")).isNotNull().isEqualTo(-1);
          assertThat(sort.getInteger("communityId")).isNotNull().isEqualTo(1);
          assertThat(sort.getInteger("taskTypeId")).isNotNull().isEqualTo(1);
          assertThat(sort.getInteger("taskId")).isNotNull().isEqualTo(-1);
          assertThat(sort.getInteger("senderId")).isNotNull().isEqualTo(1);
          assertThat(sort.getInteger("receiverId")).isNotNull().isEqualTo(1);
          assertThat(sort.getInteger("transactionLabel")).isNotNull().isEqualTo(-1);
          assertThat(sort.getInteger("transactionTs")).isNotNull().isEqualTo(1);
          assertThat(sort.getInteger("messageLabel")).isNotNull().isEqualTo(-1);
          assertThat(sort.getInteger("messageTs")).isNotNull().isEqualTo(1);
          testContext.completeNow();

        })));

  }

}
