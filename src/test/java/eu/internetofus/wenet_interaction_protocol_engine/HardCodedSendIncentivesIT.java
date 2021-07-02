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

package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.models.IncentiveTest;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.models.Message;
import io.vertx.core.Vertx;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the hard coded send incentives.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class HardCodedSendIncentivesIT {

  /**
   * Check that an incentive message is send to the user.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.HOURS)
  @Test
  public void shouldSendIncentiveMessage(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(new IncentiveTest().createModelExample(1, vertx, testContext)).onSuccess(incentive -> {

      incentive.IncentiveType = "Message";
      incentive.Badge = null;
      testContext.assertComplete(WeNetInteractionProtocolEngine.createProxy(vertx).sendIncentive(incentive))
          .onSuccess(sent -> {

            waitUntilCallbacks(incentive.AppID, callbacks -> {

              for (var i = 0; i < callbacks.size(); i++) {

                final var message = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
                if (message != null && "IncentiveMessage".equals(message.label)
                    && incentive.UserId.equals(message.receiverId)
                    && incentive.Message.content.equals(message.attributes.getString("content"))) {

                  return true;

                }
              }

              return false;

            }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> testContext.completeNow()));

          });

    });

  }

  /**
   * Check that an incentive badge is send to the user.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  public void shouldSendIncentiveBadge(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(new IncentiveTest().createModelExample(1, vertx, testContext)).onSuccess(incentive -> {

      incentive.IncentiveType = "Badge";
      incentive.Message = null;
      testContext.assertComplete(WeNetInteractionProtocolEngine.createProxy(vertx).sendIncentive(incentive))
          .onSuccess(sent -> {

            waitUntilCallbacks(incentive.AppID, callbacks -> {

              for (var i = 0; i < callbacks.size(); i++) {

                final var message = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
                if (message != null && "IncentiveBadge".equals(message.label)
                    && incentive.UserId.equals(message.receiverId)
                    && incentive.Badge.BadgeClass.equals(message.attributes.getString("badgeClass"))
                    && incentive.Badge.ImgUrl.equals(message.attributes.getString("imageUrl"))
                    && incentive.Badge.Criteria.equals(message.attributes.getString("criteria"))
                    && incentive.Badge.Message.equals(message.attributes.getString("message"))) {

                  return true;

                }
              }

              return false;

            }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> testContext.completeNow()));

          });

    });

  }

}
