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

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.HumanDescription;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.CommunityProfile;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.Message;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskType;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.Protocol;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.List;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;

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
   * The task type that will involved on the test.
   */
  protected static TaskType taskType;

  /**
   * The task that will involved on the test.
   */
  protected static Task task;

  /**
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(1, vertx, testContext).onSuccess(me -> {

      testContext.assertComplete(createUsers(MAX_USERS, vertx, testContext)).onSuccess(users -> {
        SWIPrologIT.users = users;
        SWIPrologIT.users.add(0, me);
        testContext.completeNow();
      });

    });
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
    StoreServices.storeCommunityExample(1, vertx, testContext).onSuccess(community -> {

      SWIPrologIT.community = community;
      testContext.assertComplete(WeNetService.createProxy(vertx).retrieveApp(community.appId).onSuccess(app -> {
        SWIPrologIT.app = app;
        final var appUsers = new JsonArray();
        for (final WeNetUserProfile profile : users) {

          appUsers.add(profile.id);
        }
        WeNetServiceSimulator.createProxy(vertx).addUsers(app.appId, appUsers,
            testContext.succeeding(added -> testContext.completeNow()));

      }));

    });

  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(3)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;
    StoreServices.storeTaskTypeExample(1, vertx, testContext).onSuccess(createdTaskType -> {

      SWIPrologIT.taskType = createdTaskType;
      final var task = new Task();
      task.appId = SWIPrologIT.app.appId;
      var deadlineTs = TimeManager.now() + 120;
      var startTs = deadlineTs + 30;
      var endTs = startTs + 300;
      task.attributes = new JsonObject().put("deadlineTs", deadlineTs).put("startTs", startTs).put("endTs", endTs);
      task.taskTypeId = taskType.id;
      task.goal = new HumanDescription();
      task.goal.name = "Test create task";
      task.requesterId = SWIPrologIT.users.get(0).id;
      testContext.assertComplete(WeNetTaskManager.createProxy(vertx).createTask(task)).onSuccess(createdTask -> {

        SWIPrologIT.task = createdTask;
        testContext.completeNow();
      });

    });

  }

  /**
   * Store the protocol to follow.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(4)
  public void shouldStoreProtocol(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.users != null;
    final var protocol = new Protocol();
    protocol.appId = SWIPrologIT.app.appId;
    protocol.communityId = SWIPrologIT.community.id;
    protocol.taskTypeId = SWIPrologIT.taskType.id;
    protocol.taskId = SWIPrologIT.task.id;
    protocol.norms = "whenever  myuser(X) and msg_from(X,_) thenceforth  msg_to(X,1).";
    protocol.ontology = "message(1,es,\"Hola, estamos trabajando en esta app para que pronto puedas hacer cosas mucho más interesantes.\").\n"
        + "message(1,_,\"Hi, we are working on this app so you can do much more interesting things soon.\").";
//    ProtocolsRepository.createProxy(vertx).storeProtocol(protocol,
//        testContext.succeeding(added -> testContext.completeNow()));

  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(5)
  public void shouldSendMessage(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;
    final var message = new ProtocolMessage();
    message.appId = app.appId;
    message.communityId = community.id;
    message.taskId = task.id;
    message.sender = new ProtocolAddress();
    message.sender.component = Component.USER_APP;
    message.sender.userId = users.get(0).id;
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    message.particle = "echo";
    message.content = new JsonObject();
    testContext.assertComplete(WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message))
        .onSuccess(sent -> {

          waitUntilCallbacks(SWIPrologIT.app.appId, callbacks -> {

            for (var i = 0; i < callbacks.size(); i++) {

              final var callback = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
              if (callback != null && "TextualMessage".equals(callback.label) && callback.receiverId != null
                  && callback.receiverId.equals(message.sender.userId)) {

                return true;

              }
            }

            return false;

          }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> {
            WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(SWIPrologIT.app.appId,
                testContext.succeeding(removed -> testContext.completeNow()));
          }));

        });

  }

}
