/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.profile_manager.WeNetProfileManagers.createUsers;
import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;

import java.util.List;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.CommunityProfile;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TextualMessage;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Check the integration for the SWIProlog.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
@TestMethodOrder(OrderAnnotation.class)
public class SWIPrologIT {

  /**
   * Number maximum of users to use on the test.
   */
  private static final int MAX_USERS = 6;

  /**
   * The users that will involved on the test.
   */
  protected static List<WeNetUserProfile> users;

  /**
   * The application that will involved on the test.
   */
  protected static App app;

  /**
   * The community that will involved on the test.
   */
  protected static CommunityProfile community;

  /**
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(1, vertx, testContext, testContext.succeeding(me -> {

      createUsers(MAX_USERS, vertx, testContext).onComplete(testContext.succeeding(users -> {
        SWIPrologIT.users = users;
        SWIPrologIT.users.add(0, me);
        testContext.completeNow();
      }));

    }));
  }

  /**
   * Create the app that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(2)
  public void shouldCreateApp(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.users != null;
    StoreServices.storeCommunityExample(1, vertx, testContext, testContext.succeeding(community -> {

      SWIPrologIT.community = community;
      WeNetService.createProxy(vertx).retrieveApp(community.appId, testContext.succeeding(app -> {
        SWIPrologIT.app = app;
        final var appUsers = new JsonArray();
        for (final WeNetUserProfile profile : users) {

          appUsers.add(profile.id);
        }
        WeNetServiceSimulator.createProxy(vertx).addUsers(app.appId, appUsers, testContext.succeeding(added -> testContext.completeNow()));

      }));

    }));

  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(3)
  public void shouldSendMessage(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;
    final var message = new ProtocolMessage();
    message.appId = app.appId;
    message.communityId = community.id;
    message.sender = new ProtocolAddress();
    message.sender.component = Component.USER_APP;
    message.sender.userId = users.get(0).id;
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    message.particle = "echo";
    message.content = new JsonObject();
    WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message, testContext.succeeding(sent -> {

      waitUntilCallbacks(SWIPrologIT.app.appId, callbacks -> {

        for (var i = 0; i < callbacks.size(); i++) {

          final var callback = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
          if (callback != null && callback.recipientId != null && callback.recipientId.equals(message.sender.userId)) {

            return true;

          }
        }

        return false;

      }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(SWIPrologIT.app.appId, testContext.succeeding(removed -> testContext.completeNow()));
      }));

    }));

  }

}
